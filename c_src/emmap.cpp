// vim:ts=2:sw=2:et

#include "erl_nif_compat.h"
#include <sys/mman.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <atomic>

static ErlNifResourceType* MMAP_RESOURCE;

#ifndef MAP_NOCACHE
/* No MAP_NOCACHE on Linux - just bypass this option */
# define MAP_NOCACHE (0)
#endif

 // MAP_ANON is deprecated on Linux, and MAP_ANONYMOUS is not present on Mac
 #ifndef MAP_ANONYMOUS
 # define MAP_ANONYMOUS MAP_ANON
 #endif

typedef struct
{
  size_t        position;
  int           direct;
  int           prot;
  bool          closed;
  bool          debug;
  ErlNifRWLock* rwlock;
  void*         mem;
  size_t        len;
  bool          auto_unlink;
  char          path[1024];
} mhandle;

static int on_load(ErlNifEnv*, void**, ERL_NIF_TERM);
static int on_upgrade(ErlNifEnv* env, void** priv_data, void**, ERL_NIF_TERM load_info);

static int emmap_unmap(mhandle *handle, bool from_dtor)
{
  if (handle->mem != 0) {
    if (from_dtor || !handle->direct) {
      int result = munmap(handle->mem, handle->len);
      handle->mem = 0;
      return result;
    } else {
      handle->closed = true;
    }
  }
  return 0;
}

void emmap_dtor(ErlNifEnv* env, void* arg)
{
  mhandle* handle = (mhandle*)arg;
  emmap_unmap(handle, true);

  // only the destructor destroys the rwlock
  if (handle->rwlock != 0) enif_rwlock_destroy(handle->rwlock);
}

#define RW_UNLOCK if (handle->rwlock != 0) enif_rwlock_rwunlock(handle->rwlock)
#define RW_LOCK   if (handle->rwlock != 0) enif_rwlock_rwlock(handle->rwlock)
#define R_UNLOCK  if (handle->rwlock != 0) enif_rwlock_runlock(handle->rwlock)
#define R_LOCK    if (handle->rwlock != 0) enif_rwlock_rlock(handle->rwlock)

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_DEBUG;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_ADDRESS;
static ERL_NIF_TERM ATOM_LOCK;
static ERL_NIF_TERM ATOM_NOLOCK;
static ERL_NIF_TERM ATOM_DIRECT;
static ERL_NIF_TERM ATOM_READ;
static ERL_NIF_TERM ATOM_WRITE;
static ERL_NIF_TERM ATOM_CREATE;
static ERL_NIF_TERM ATOM_TRUNCATE;
static ERL_NIF_TERM ATOM_CHMOD;
static ERL_NIF_TERM ATOM_NONE;
static ERL_NIF_TERM ATOM_PRIVATE;
static ERL_NIF_TERM ATOM_POPULATE;
static ERL_NIF_TERM ATOM_SHARED;
static ERL_NIF_TERM ATOM_SHARED_VALIDATE;
static ERL_NIF_TERM ATOM_SYNC;
static ERL_NIF_TERM ATOM_ANON;
static ERL_NIF_TERM ATOM_FILE;
static ERL_NIF_TERM ATOM_FIXED;
static ERL_NIF_TERM ATOM_NOCACHE;
static ERL_NIF_TERM ATOM_NORESERVE;
static ERL_NIF_TERM ATOM_UNINITIALIZED;
static ERL_NIF_TERM ATOM_AUTO_UNLINK;
static ERL_NIF_TERM ATOM_ADD;
static ERL_NIF_TERM ATOM_SUB;
static ERL_NIF_TERM ATOM_BAND;
static ERL_NIF_TERM ATOM_BOR;
static ERL_NIF_TERM ATOM_BXOR;
static ERL_NIF_TERM ATOM_XCHG;

static ERL_NIF_TERM ATOM_BOF;
static ERL_NIF_TERM ATOM_CUR;
static ERL_NIF_TERM ATOM_EOF;

static ERL_NIF_TERM emmap_open         (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_read         (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_read_line    (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_close        (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_pread        (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_pwrite       (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_position     (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic      (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_read (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_write(ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);

extern "C" {

  static ErlNifFunc nif_funcs[] =
  {
    {"open_nif",          4, emmap_open},
    {"close_nif",         1, emmap_close},
    {"pread_nif",         3, emmap_pread},
    {"pwrite_nif",        3, emmap_pwrite},
    {"patomic_nif",       4, emmap_patomic},
    {"patomic_read_nif",  2, emmap_patomic_read},
    {"patomic_write_nif", 3, emmap_patomic_write},
    {"position_nif",      3, emmap_position},
    {"read_nif",          2, emmap_read},
    {"read_line_nif",     1, emmap_read_line},
  };

};

ERL_NIF_INIT(emmap, nif_funcs, &on_load, nullptr, on_upgrade, nullptr);

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  MMAP_RESOURCE = enif_open_resource_type_compat(env, "mmap_resource", &emmap_dtor, flags, 0);

  ATOM_OK           = enif_make_atom(env, "ok");
  ATOM_TRUE         = enif_make_atom(env, "true");
  ATOM_FALSE        = enif_make_atom(env, "false");
  ATOM_ERROR        = enif_make_atom(env, "error");
  ATOM_DEBUG        = enif_make_atom(env, "debug");

  ATOM_ADDRESS          = enif_make_atom(env, "address");
  ATOM_DIRECT           = enif_make_atom(env, "direct");
  ATOM_READ             = enif_make_atom(env, "read");
  ATOM_WRITE            = enif_make_atom(env, "write");
  ATOM_CREATE           = enif_make_atom(env, "create");
  ATOM_TRUNCATE         = enif_make_atom(env, "truncate");
  ATOM_CHMOD            = enif_make_atom(env, "chmod");
  ATOM_NONE             = enif_make_atom(env, "none");
  ATOM_PRIVATE          = enif_make_atom(env, "private");
  ATOM_POPULATE         = enif_make_atom(env, "populate");
  ATOM_SHARED           = enif_make_atom(env, "shared");
  ATOM_SHARED_VALIDATE  = enif_make_atom(env, "shared_validate");
  ATOM_SYNC             = enif_make_atom(env, "sync");
  ATOM_ANON             = enif_make_atom(env, "anon");
  ATOM_FILE             = enif_make_atom(env, "file");
  ATOM_FIXED            = enif_make_atom(env, "fixed");
  ATOM_NOCACHE          = enif_make_atom(env, "nocache");
  ATOM_NORESERVE        = enif_make_atom(env, "noreserve");
  ATOM_AUTO_UNLINK      = enif_make_atom(env, "auto_unlink");
  ATOM_UNINITIALIZED    = enif_make_atom(env, "uninitialized");

  ATOM_BOF              = enif_make_atom(env, "bof");
  ATOM_CUR              = enif_make_atom(env, "cur");
  ATOM_EOF              = enif_make_atom(env, "eof");

  ATOM_LOCK             = enif_make_atom(env, "lock");
  ATOM_NOLOCK           = enif_make_atom(env, "nolock");

  ATOM_ADD              = enif_make_atom(env, "add");
  ATOM_SUB              = enif_make_atom(env, "sub");
  ATOM_BAND             = enif_make_atom(env, "band");
  ATOM_BOR              = enif_make_atom(env, "bor");
  ATOM_BXOR             = enif_make_atom(env, "bxor");
  ATOM_XCHG             = enif_make_atom(env, "xchg");

  return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void**, ERL_NIF_TERM load_info)
{
    return on_load(env, priv_data, load_info);
}

static ERL_NIF_TERM describe_error(ErlNifEnv* env, int err) {
  switch (err) {
    case EAGAIN:
      return enif_make_atom(env, "eagain");
    case EINVAL:
      return enif_make_atom(env, "einval");
    case ENOSPC:
      return enif_make_atom(env, "enospc");
    case ENOENT:
      return enif_make_atom(env, "enoent");
    case ENOMEM:
      return enif_make_atom(env, "enomem");
    case EACCES:
      return enif_make_atom(env, "eacces");
    case EBADF:
      return enif_make_atom(env, "ebadf");
    case ENODEV:
      return enif_make_atom(env, "enodev");
    case ENXIO:
      return enif_make_atom(env, "enxio");
    case EOVERFLOW:
      return enif_make_atom(env, "eoverflow");
    case EOPNOTSUPP:
      return enif_make_atom(env, "eopnotsupp");
  }
  return enif_make_tuple2(env,
                          enif_make_atom(env, "errno"),
                          enif_make_int(env, err));
}

static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, int err) {
  return enif_make_tuple2(env, ATOM_ERROR, describe_error(env, err));
}

static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, const char* err) {
  return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, err, ERL_NIF_LATIN1));
}

template <typename... Args>
static void debug(mhandle* h, const char* fmt, Args&&... args)
{
  if (h->debug)
    fprintf(stderr, fmt, std::forward<Args>(args)...);
}

static bool decode_flags(ErlNifEnv* env, ERL_NIF_TERM list, int* prot, int* flags, 
                         long* open_flags,  long* mode, bool* direct, bool* lock,
                         bool* auto_unlink, size_t* address, bool* debug)
{
  bool l = true;
  bool d = false;
  int f = MAP_FILE;
  int p = 0;
  int arity;
  const ERL_NIF_TERM* tuple;

  *auto_unlink = false;
  *address     = 0x0;
  *open_flags  = O_RDONLY;
  *mode        = 0600;
  *debug       = false;

  ERL_NIF_TERM head;
  while (enif_get_list_cell(env, list, &head, &list)) {

    if (enif_is_identical(head, ATOM_READ)) {
      p |= PROT_READ;
    } else if (enif_is_identical(head, ATOM_DEBUG)) {
      *debug = true;
    } else if (enif_is_identical(head, ATOM_CREATE)) {
      *open_flags |= O_CREAT;
    } else if (enif_is_identical(head, ATOM_TRUNCATE)) {
      *open_flags |= O_TRUNC;
    } else if (enif_is_identical(head, ATOM_DIRECT)) {
      d = true;
    } else if (enif_is_identical(head, ATOM_LOCK)) {
      l = true;
    } else if (enif_is_identical(head, ATOM_NOLOCK)) {
      l = false;
    } else if (enif_is_identical(head, ATOM_WRITE)) {
      p |= PROT_WRITE;
      *open_flags |= O_RDWR;
//    } else if (enif_is_identical(head, ATOM_NONE)) {
//    p |= PROT_NONE;

    } else if (enif_is_identical(head, ATOM_PRIVATE)) {
      f |= MAP_PRIVATE;
#if ERL_NIF_MINOR_VERSION > 4
  #ifdef MAP_POPULATE
    } else if (enif_is_identical(head, ATOM_POPULATE)) {
      f |= MAP_POPULATE;
  #endif
#endif
    } else if (enif_is_identical(head, ATOM_SHARED_VALIDATE)) {
      f |= MAP_SHARED_VALIDATE;
    } else if (enif_is_identical(head, ATOM_SHARED)) {
      f |= MAP_SHARED;
    } else if (enif_is_identical(head, ATOM_ANON)) {
      f |= MAP_ANONYMOUS;
    } else if (enif_is_identical(head, ATOM_SYNC)) {
      f |= MAP_SYNC;
    } else if (enif_is_identical(head, ATOM_FIXED)) {
      f |= MAP_FIXED;
//  } else if (enif_is_identical(head, ATOM_FILE)) {
//    f |= MAP_FILE;
    } else if (enif_is_identical(head, ATOM_NOCACHE)) {
      f |= MAP_NOCACHE;
    } else if (enif_is_identical(head, ATOM_UNINITIALIZED)) {
      f |= MAP_NOCACHE;
#ifdef MAP_NORESERVE
    } else if (enif_is_identical(head, ATOM_NORESERVE)) {
      f |= MAP_NORESERVE;
#endif
    } else if (enif_is_identical(head, ATOM_AUTO_UNLINK)) {
      *auto_unlink = true;
    } else if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2) {
      if (enif_is_identical  (tuple[0], ATOM_ADDRESS) &&
          enif_get_ulong(env, tuple[1], address)) {
        // If address is given, set the "MAP_FIXED" option
        if (*address) f |= MAP_FIXED;
        continue;
      } else if (enif_is_identical  (tuple[0], ATOM_CHMOD) &&
          enif_get_long(env, tuple[1], mode)) {
        continue;
      } else
        return false;
    } else {
      return false;
    }
  }

  // direct cannot be write
  //if (d & ((p & PROT_WRITE) != 0))
  //  return 0;

  // default to private
  if ((f & (MAP_SHARED|MAP_PRIVATE)) == 0)
    f |= MAP_PRIVATE;

  // default to read-only
  if ((p & (PROT_READ|PROT_WRITE)) == 0)
    p |= PROT_READ;

  *flags  = f;
  *prot   = p;
  *direct = d;
  *lock   = l;

  return true;
}

static ERL_NIF_TERM emmap_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int flags;
  int prot;
  long open_flags;
  long mode;
  bool direct, lock, auto_unlink;
  unsigned long int len;
  unsigned long int offset;
  size_t address;

#ifndef NDEBUG
  if (sizeof(long int) != sizeof(size_t))
    abort();
#endif
  mhandle* handle = (mhandle*)enif_alloc_resource_compat(env, MMAP_RESOURCE,
                                                           sizeof(mhandle));

  if (argc != 4
      || !enif_get_string(env, argv[0], handle->path, 1024, ERL_NIF_LATIN1)
      || !enif_get_ulong(env, argv[1], &offset)
      || !enif_get_ulong(env, argv[2], &len)
      || !decode_flags(env, argv[3], &prot, &flags, &open_flags, &mode, &direct,
                       &lock, &auto_unlink, &address, &handle->debug))
    return enif_make_badarg(env);

  int fd = -1;

  if ((flags & MAP_ANON) == 0) {
    bool exists = access(handle->path, F_OK) == 0;

    if (!exists) {
      if (len == 0)
        return make_error_tuple(env, "Missing {size, N} option");
      if ((open_flags & O_CREAT) == 0)
        return make_error_tuple(env, "Missing 'create' option");
    }

    fd = open(handle->path, open_flags, (mode_t)mode);

    if (fd < 0) {
      debug(handle, "open: %s\r\n", strerror(errno));
      return make_error_tuple(env, errno);
    }

    off_t fsize = 0;
    struct stat st;
    if (fstat(fd, &st) < 0) {
      debug(handle, "fstat: %s\r\n", strerror(errno));
      int err = errno;
      close(fd);
      return make_error_tuple(env, err);
    } else {
      fsize = st.st_size;
    }

    if (exists && len > 0 && fsize != long(len) && fsize > 0) {
      char buf[128];
      snprintf(buf, sizeof(buf), "File has different size (%ld) than requested (%ld)", fsize, len);
      close(fd);
      return make_error_tuple(env, buf);
    }

    // Stretch the file size to the requested size
    if ((!exists && ((open_flags & (O_CREAT|O_TRUNC)) > 0)) ||
         (exists && fsize == 0)) {
      bool is_ok = ftruncate(fd, 0)   == 0 &&
                   ftruncate(fd, len) == 0;
      if (!is_ok) {
        debug(handle, "ftruncate: %s\r\n", strerror(errno));
        int err = errno;
        close(fd);
        return make_error_tuple(env, err);
      }
    }
  }

  off_t pa_offset = 0;
  if (offset > 0) {
    pa_offset = offset & ~(sysconf(_SC_PAGE_SIZE) - 1); // offset for mmap() must be page aligned

    if (offset >= len) {
      if (fd >= 0) close(fd);
      char buf[128];
      snprintf(buf, sizeof(buf), "Offset %ld is past end of file", offset);
      return make_error_tuple(env, buf);
    }
  }
  void* res = mmap((void*)address, (size_t)len + offset - pa_offset, prot, flags, fd,
                   (size_t)pa_offset);
  if (res == MAP_FAILED)
    return make_error_tuple(env, errno);

  if (fd >= 0)
    close(fd);

  handle->rwlock = lock ? enif_rwlock_create((char*)"mmap") : 0;

  handle->prot = prot;
  handle->mem = res;
  handle->len = len;
  handle->closed = false;
  handle->direct = direct;
  handle->position = 0;
  handle->auto_unlink = auto_unlink;

  ERL_NIF_TERM resource = enif_make_resource(env, handle);
  enif_release_resource_compat(env, handle);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), resource);
}

static ERL_NIF_TERM emmap_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle *handle;
  if (argc==1 && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle))
    {
      int res;
      RW_LOCK;
      res = emmap_unmap(handle, false);
      RW_UNLOCK;

      if(handle->auto_unlink && unlink(handle->path) != 0)
        return make_error_tuple(env, errno);

      if (res == 0) {
        return ATOM_OK;
      }

      return make_error_tuple(env, errno);
    }
  else
    {
      return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM emmap_pread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long pos, bytes;
  mhandle *handle;
  if (argc==3
      && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      && enif_get_ulong(env, argv[1], &pos)
      && enif_get_ulong(env, argv[2], &bytes)
      && pos <= handle->len
      )
    {
      // Adjust bytes to behave like original file:pread/3
      if (pos + bytes > handle->len) bytes = handle->len - pos;

      ErlNifBinary bin;

      if ((handle->prot & PROT_READ) == 0) {
        return make_error_tuple(env, EACCES);
      }

      // if this mmap is direct, use a resource binary
      if (handle->direct) {

        ERL_NIF_TERM res = enif_make_resource_binary
          (env, handle, (void*) (((char*)handle->mem) + pos), bytes);

        return enif_make_tuple2(env, ATOM_OK, res);

      } else {

        // When it is non-direct, we have to allocate the binary
        if (!enif_alloc_binary((size_t) bytes, &bin)) {
          return make_error_tuple(env, ENOMEM);
        }

        R_LOCK;
        if (handle->closed) {
          R_UNLOCK;
          return enif_make_badarg(env);
        }
        memcpy(bin.data, (void*) (((char*)handle->mem) + pos), bytes);
        R_UNLOCK;

        ERL_NIF_TERM res = enif_make_binary(env, &bin);
        return enif_make_tuple2(env, ATOM_OK, res);
      }
    }
  else
    {
      return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM emmap_pwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  unsigned long pos;
  mhandle *handle;
  if (argc==3
      && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      && enif_get_ulong(env, argv[1], &pos)
      && enif_inspect_binary(env, argv[2], &bin)
      && (pos + bin.size) <= handle->len
      )
    {

      if ((handle->prot & PROT_WRITE) == 0) {
        return make_error_tuple(env, EACCES);
      }

      RW_LOCK;
      if (handle->closed) {
        RW_UNLOCK;
        return enif_make_badarg(env);
      } else {
        memcpy((void*) (((char*)handle->mem) + pos), bin.data, bin.size);
        RW_UNLOCK;
      }

      return ATOM_OK;
    }
  else
    {
      return enif_make_badarg(env);
    }
}

/// Atomically read a 64-bit value from the memoty at given position
/// Args:   Handle, Position::integer()
/// Return: Value::integer()
static ERL_NIF_TERM emmap_patomic_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;

  if (!(argc==2
        && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
        && enif_get_ulong   (env, argv[1], &pos)
        && (pos + 8) <= handle->len
     ))
    return enif_make_badarg(env);

  if (handle->closed)
    return enif_make_badarg(env);


  void* mem = (char*)handle->mem + pos;
  auto  p   = static_cast<std::atomic<int64_t>*>(mem);
  long  res = p->load(std::memory_order_relaxed);

  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically write a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Value::integer()
static ERL_NIF_TERM emmap_patomic_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (!(argc==3
        && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
        && enif_get_ulong   (env, argv[1], &pos)
        && enif_get_long    (env, argv[2], &value)
        && (pos + 8) <= handle->len
     ))
    return enif_make_badarg(env);

  if (handle->closed)
    return enif_make_badarg(env);

  void* mem = (char*)handle->mem + pos;
  ((std::atomic<int64_t>*)mem)->store(value);

  return ATOM_OK;
}

/// Atomically add a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Op, Increment::integer()
///           where Op :: add|sub|and|or|xor|xchg
/// Return: OldValue::integer()
static ERL_NIF_TERM emmap_patomic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (!(argc==4
        && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
        && enif_get_ulong   (env, argv[1], &pos)
        && enif_is_atom     (env, argv[2])
        && enif_get_long    (env, argv[3], &value)
        && (pos + 8) <= handle->len
     ))
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error_tuple(env, EACCES);

  if (handle->closed)
    return enif_make_badarg(env);

  void*         mem = (char*)handle->mem + pos;
  long          res;
  ERL_NIF_TERM  op  = argv[2];

  if (op == ATOM_ADD)
    res = ((std::atomic<int64_t>*)mem)->fetch_add(value, std::memory_order_relaxed);
  else if (op == ATOM_SUB)
    res = ((std::atomic<int64_t>*)mem)->fetch_sub(value, std::memory_order_relaxed);
  else if (op == ATOM_BAND)
    // Atomically replaces the cur value with the result of bitwise (value & arg)
    res = ((std::atomic<int64_t>*)mem)->fetch_and(value, std::memory_order_relaxed);
  else if (op == ATOM_BOR)
    // Atomically replaces the cur value with the result of bitwise (value | arg)
    res = ((std::atomic<int64_t>*)mem)->fetch_or(value, std::memory_order_relaxed);
  else if (op == ATOM_BXOR)
    // Atomically replaces the cur value with the result of bitwise (value ^ arg)
    res = ((std::atomic<int64_t>*)mem)->fetch_xor(value, std::memory_order_relaxed);
  else if (op == ATOM_XCHG)
    // Atomically replaces the cur value with the arg
    res = ((std::atomic<int64_t>*)mem)->exchange(value, std::memory_order_acq_rel);
  else
    return enif_make_badarg(env);

  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

static ERL_NIF_TERM emmap_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle *handle;
  unsigned long bytes;

  if (enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      && enif_get_ulong(env, argv[1], &bytes)) {


    RW_LOCK;

    if (handle->position == handle->len) {
      RW_UNLOCK;
      return ATOM_EOF;
    }

    unsigned long new_pos = handle->position + bytes;
    if (new_pos > handle->len) { new_pos = handle->len; }
    long size = new_pos - handle->position;
    long start = handle->position;
    handle->position = new_pos;
    RW_UNLOCK;

    if (handle->direct) {

      ERL_NIF_TERM res = enif_make_resource_binary
        (env, handle, (void*) (((char*)handle->mem) + start), size);

      return enif_make_tuple2(env, ATOM_OK, res);

    } else {

      ErlNifBinary bin;
      // When it is non-direct, we have to allocate the binary
      if (!enif_alloc_binary((size_t) size, &bin)) {
        return make_error_tuple(env, ENOMEM);
      }

      memcpy(bin.data, (void*) (((char*)handle->mem) + start), size);

      ERL_NIF_TERM res = enif_make_binary(env, &bin);
      return enif_make_tuple2(env, ATOM_OK, res);
    }

  } else {
    return enif_make_badarg(env);
  }
}

static ERL_NIF_TERM emmap_read_line(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle *handle;

  if (enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)) {

    RW_LOCK;

    if (handle->position == handle->len) {
      RW_UNLOCK;
      return ATOM_EOF;
    }

    long start = handle->position;
    char *current = ((char*)handle->mem) + handle->position;
    long linelen = 0; // Length of line without trailing \n
    long no_crlf_len = 0;
    bool have_cr = false;
    bool got_eof = false;

    // Read buffer until \n or EOF is reached
    while (*current != '\n') {
      handle->position ++;
      current ++;
      if (handle->position == handle->len) {
        got_eof = true;
        break;
      }
    }
    // Step to next byte if EOF is not reached
    if (not got_eof) {
      handle->position ++;
    }

    no_crlf_len = linelen = handle->position - start;

    if (not got_eof) {
      // Found LF -- exclude it from line
      no_crlf_len --;
      // If line length before \n is non-zero check if we have \r before \n
      if ((no_crlf_len > 0) && (*(current - 1) == '\r')) {
        have_cr = true;
        no_crlf_len --;
      }
    }

    RW_UNLOCK;

    // We must not include CR before LF in result, so use direct only without CR
    if ((handle->direct) && (not have_cr)) {

      // Include trailing LF if we have it
      ERL_NIF_TERM res = enif_make_resource_binary
        (env, handle, (void*) (((char*)handle->mem) + start), linelen);

      return enif_make_tuple2(env, ATOM_OK, res);

    } else {
      if (not got_eof) linelen = no_crlf_len + 1;

      ErlNifBinary bin;
      // When it is non-direct, we have to allocate the binary
      if (!enif_alloc_binary((size_t) linelen, &bin)) {
        return make_error_tuple(env, ENOMEM);
      }

      memcpy(bin.data, (void*) (((char*)handle->mem) + start), no_crlf_len);
      // Set trailing \n if needed
      if (not got_eof) *(((char*)bin.data) + no_crlf_len) = '\n';

      ERL_NIF_TERM res = enif_make_binary(env, &bin);
      return enif_make_tuple2(env, ATOM_OK, res);
    }

  } else {
    return enif_make_badarg(env);
  }
}

static ERL_NIF_TERM emmap_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle *handle;
  long position;
  long relpos;
  if (argc==3
      && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      && enif_get_long(env, argv[2], &relpos)
      && (argv[1] == ATOM_CUR || argv[1] == ATOM_BOF || argv[1] == ATOM_EOF))
    {
      RW_LOCK;

      if (argv[1] == ATOM_BOF) {
        position = 0L + relpos;
      } else if (argv[1] == ATOM_CUR) {
        position = handle->position + relpos;
      } else if (argv[1] == ATOM_EOF) {
        position = handle->len - relpos;
      } else {
        position = -1;
      }

      if (position < 0L || ((unsigned long)position) > handle->len) {
        RW_UNLOCK;
        return enif_make_badarg(env);
      }

      handle->position = position;
      RW_UNLOCK;

      return enif_make_tuple2(env, ATOM_OK, enif_make_ulong(env, position));
    }
  else
    {
      return enif_make_badarg(env);
    }
}
