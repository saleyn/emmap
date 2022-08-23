// vim:ts=2:sw=2:et

#include <sys/mman.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <atomic>
#include <memory>
#include <cassert>
#include <list>
#include <erl_nif.h>

static ErlNifResourceType* MMAP_RESOURCE;

#ifndef MAP_NOCACHE
/* No MAP_NOCACHE on Linux - just bypass this option */
# define MAP_NOCACHE (0)
#endif

// MAP_ANON is deprecated on Linux, and MAP_ANONYMOUS is not present on Mac
#ifndef MAP_ANONYMOUS
# define MAP_ANONYMOUS MAP_ANON
#endif

#if (__GLIBC__ == 2 && __GLIBC_MINOR >= 32) || (__GLIBC__ > 2)
# define strerror_compat strerrordesc_np
#else
# define strerror_compat strerror
#endif

template <typename... Args>
static void debug(bool dbg, const char* fmt, Args&&... args)
{
  if (dbg)
    fprintf(stderr, fmt, std::forward<Args>(args)...);
}

struct mhandle
{
  mhandle(void* mem,   const char* file, size_t size, bool direct,
          bool fixed_size,  size_t max_inc_size,
          bool auto_unlink, int    prot, bool   lock, bool dbg)
    : position(0)
    , direct(direct)
    , prot(prot)
    , dbg(dbg)
    , rwlock(lock ? enif_rwlock_create((char*)"mmap") : nullptr)
    , mem(mem)
    , direct_mem()
    , len(size)
    , max_inc_size(max_inc_size)
    , fixed_size(fixed_size)
    , auto_unlink(auto_unlink)
  {
    snprintf(path, sizeof(path), "%s", file);
  }

  ~mhandle()
  {
    unmap(true);

    if (direct) {
      for (auto& slot : direct_mem) {
        debug(dbg, "Unmapping direct memory %p of size %lu\r\n", slot.first, slot.second);
        munmap(slot.first, slot.second);
      }
      direct_mem.clear();
    }

    if (rwlock != 0)
      enif_rwlock_destroy(rwlock);
  }

  bool unmap(bool destruct)
  {
    auto result = true;

    if (mem) {
      debug(dbg, "Unmapping memory %p of size %lu\r\n", mem, len);
      if (destruct || !direct) {
        debug(dbg, "Releasing memory map %p of size %lu\r\n", mem, len);
        result = munmap(mem, len) == 0;
      } else if (direct) {
        // We must not unmap the memory that might be in use by some binaries
        // until the destructor is called.
        debug(dbg, "Preserving memory map %p of size %lu\r\n", mem, len);
        direct_mem.push_back(MemSlot(mem, len));
      }

      mem = nullptr;
    }

    if (auto_unlink) {
      debug(dbg, "Removing memory mapped file %s\r\n", path);
      unlink(path);
    }

    return result;
  }

  bool closed() const { return mem == nullptr; }

  using MemSlot = std::pair<void*, size_t>;
  using MemList = std::list<MemSlot>;

  size_t          position;
  bool            direct;
  int             prot;
  bool            dbg;
  ErlNifRWLock*   rwlock;
  void*           mem;
  MemList         direct_mem;     // Stash of allocated mem segments used in direct mode
  size_t          len;
  size_t          max_inc_size;   // Below this threshold, when resizing, the size will double
  bool            fixed_size;     // Don't allow memory to be resized
  bool            auto_unlink;
  char            path[1024];
};

template <typename... Args>
static void debug(mhandle* h, const char* fmt, Args&&... args)
{
  debug(h->dbg, fmt, std::forward<Args>(args)...);
}

static int on_load(ErlNifEnv*, void**, ERL_NIF_TERM);
static int on_upgrade(ErlNifEnv* env, void** priv_data, void**, ERL_NIF_TERM load_info);

void emmap_dtor(ErlNifEnv* env, void* arg)
{
  assert(arg);
  static_cast<mhandle*>(arg)->~mhandle();  // Call the destructor
}

#define RW_UNLOCK if (handle->rwlock != 0) enif_rwlock_rwunlock(handle->rwlock)
#define RW_LOCK   if (handle->rwlock != 0) enif_rwlock_rwlock(handle->rwlock)
#define R_UNLOCK  if (handle->rwlock != 0) enif_rwlock_runlock(handle->rwlock)
#define R_LOCK    if (handle->rwlock != 0) enif_rwlock_rlock(handle->rwlock)

static ERL_NIF_TERM ATOM_ADDRESS;
static ERL_NIF_TERM ATOM_ALIGNMENT;
static ERL_NIF_TERM ATOM_ANON;
static ERL_NIF_TERM ATOM_AUTO_UNLINK;
static ERL_NIF_TERM ATOM_BOF;
static ERL_NIF_TERM ATOM_CANNOT_CREATE_MAP;
static ERL_NIF_TERM ATOM_CHMOD;
static ERL_NIF_TERM ATOM_CLOSED;
static ERL_NIF_TERM ATOM_CREATE;
static ERL_NIF_TERM ATOM_CUR;
static ERL_NIF_TERM ATOM_DEBUG;
static ERL_NIF_TERM ATOM_DIRECT;
static ERL_NIF_TERM ATOM_EACCES;
static ERL_NIF_TERM ATOM_ENOMEM;
static ERL_NIF_TERM ATOM_EOF;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_EXIST;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_FILE;
static ERL_NIF_TERM ATOM_FIXED;
static ERL_NIF_TERM ATOM_FIXED_SIZE;
static ERL_NIF_TERM ATOM_LOCK;
static ERL_NIF_TERM ATOM_MAX_INC_SIZE;
static ERL_NIF_TERM ATOM_NOCACHE;
static ERL_NIF_TERM ATOM_NOLOCK;
static ERL_NIF_TERM ATOM_NONE;
static ERL_NIF_TERM ATOM_NORESERVE;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_POPULATE;
static ERL_NIF_TERM ATOM_PRIVATE;
static ERL_NIF_TERM ATOM_READ;
static ERL_NIF_TERM ATOM_SHARED;
#ifndef __OSX__
static ERL_NIF_TERM ATOM_SHARED_VALIDATE;
#endif
static ERL_NIF_TERM ATOM_SIZE;
#ifndef __OSX__
static ERL_NIF_TERM ATOM_SYNC;
#endif
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_TRUNCATE;
static ERL_NIF_TERM ATOM_UNINITIALIZED;
static ERL_NIF_TERM ATOM_WRITE;

static ERL_NIF_TERM emmap_open             (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_resize           (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_read             (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_read_line        (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_close            (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_pread            (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_pwrite           (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_position         (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_add      (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_sub      (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_and      (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_or       (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_xor      (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_xchg     (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_cas      (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_read_int (ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM emmap_patomic_write_int(ErlNifEnv*, int argc, const ERL_NIF_TERM argv[]);

extern "C" {

  static ErlNifFunc nif_funcs[] =
  {
    {"open_nif",              4, emmap_open},
    {"close_nif",             1, emmap_close},
    {"resize_nif",            2, emmap_resize},
    {"pread_nif",             3, emmap_pread},
    {"pwrite_nif",            3, emmap_pwrite},
    {"patomic_read_int_nif",  2, emmap_patomic_read_int},
    {"patomic_write_int_nif", 3, emmap_patomic_write_int},
    {"patomic_add_nif",       3, emmap_patomic_add},
    {"patomic_sub_nif",       3, emmap_patomic_sub},
    {"patomic_and_nif",       3, emmap_patomic_and},
    {"patomic_or_nif",        3, emmap_patomic_or},
    {"patomic_xor_nif",       3, emmap_patomic_xor},
    {"patomic_xchg_nif",      3, emmap_patomic_xchg},
    {"patomic_cas_nif",       4, emmap_patomic_cas},
    {"position_nif",          3, emmap_position},
    {"read_nif",              2, emmap_read},
    {"read_line_nif",         1, emmap_read_line},
  };

};

ERL_NIF_INIT(emmap, nif_funcs, &on_load, nullptr, on_upgrade, nullptr);

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  MMAP_RESOURCE = enif_open_resource_type(env, nullptr, "mmap_resource", &emmap_dtor, flags, 0);

  ATOM_ADDRESS          = enif_make_atom(env, "address");
  ATOM_ALIGNMENT        = enif_make_atom(env, "alignment");
  ATOM_ANON             = enif_make_atom(env, "anon");
  ATOM_AUTO_UNLINK      = enif_make_atom(env, "auto_unlink");
  ATOM_BOF              = enif_make_atom(env, "bof");
  ATOM_CANNOT_CREATE_MAP= enif_make_atom(env, "cannot_create_map");
  ATOM_CHMOD            = enif_make_atom(env, "chmod");
  ATOM_CLOSED           = enif_make_atom(env, "closed");
  ATOM_CREATE           = enif_make_atom(env, "create");
  ATOM_CUR              = enif_make_atom(env, "cur");
  ATOM_DEBUG            = enif_make_atom(env, "debug");
  ATOM_DIRECT           = enif_make_atom(env, "direct");
  ATOM_EACCES           = enif_make_atom(env, "eacces");
  ATOM_ENOMEM           = enif_make_atom(env, "enomem");
  ATOM_EOF              = enif_make_atom(env, "eof");
  ATOM_ERROR            = enif_make_atom(env, "error");
  ATOM_EXIST            = enif_make_atom(env, "exist");
  ATOM_FALSE            = enif_make_atom(env, "false");
  ATOM_FILE             = enif_make_atom(env, "file");
  ATOM_FIXED            = enif_make_atom(env, "fixed");
  ATOM_FIXED_SIZE       = enif_make_atom(env, "fixed_size");
  ATOM_LOCK             = enif_make_atom(env, "lock");
  ATOM_MAX_INC_SIZE     = enif_make_atom(env, "max_inc_size");
  ATOM_NOCACHE          = enif_make_atom(env, "nocache");
  ATOM_NOLOCK           = enif_make_atom(env, "nolock");
  ATOM_NONE             = enif_make_atom(env, "none");
  ATOM_NORESERVE        = enif_make_atom(env, "noreserve");
  ATOM_OK               = enif_make_atom(env, "ok");
  ATOM_POPULATE         = enif_make_atom(env, "populate");
  ATOM_PRIVATE          = enif_make_atom(env, "private");
  ATOM_READ             = enif_make_atom(env, "read");
  ATOM_SHARED           = enif_make_atom(env, "shared");
#ifndef __OSX__
  ATOM_SHARED_VALIDATE  = enif_make_atom(env, "shared_validate");
#endif
  ATOM_SIZE             = enif_make_atom(env, "size");
#ifndef __OSX__
  ATOM_SYNC             = enif_make_atom(env, "sync");
#endif
  ATOM_TRUE             = enif_make_atom(env, "true");
  ATOM_TRUNCATE         = enif_make_atom(env, "truncate");
  ATOM_UNINITIALIZED    = enif_make_atom(env, "uninitialized");
  ATOM_WRITE            = enif_make_atom(env, "write");

  return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void**, ERL_NIF_TERM load_info)
{
    return on_load(env, priv_data, load_info);
}

static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, int err) {
  return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, strerror_compat(err), ERL_NIF_LATIN1));
}

static ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, const char* err) {
  return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, err, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM err_atom) {
  return enif_make_tuple2(env, ATOM_ERROR, err_atom);
}

static bool decode_flags(ErlNifEnv* env, ERL_NIF_TERM list, int* prot, int* flags, 
                         long* open_flags,  long* mode, bool* direct, bool* lock,
                         bool* auto_unlink, size_t* address, bool* debug,
                         size_t* max_inc_size, bool* fixed_size)
{
  bool   l = true;
  bool   d = false;
  int    f = MAP_FILE;
  int    p = PROT_READ;
  int    arity;
  size_t tmp;
  const  ERL_NIF_TERM* tuple;

  *auto_unlink  = false;
  *address      = 0x0;
  *open_flags   = O_RDONLY;
  *mode         = 0600;
  *debug        = false;
  *max_inc_size = 64*1024*1024;
  *fixed_size   = false;
  *prot         = 0;
  *flags        = 0;

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
    } else if (enif_is_identical(head, ATOM_PRIVATE)) {
      f |= MAP_PRIVATE;
#if ERL_NIF_MINOR_VERSION > 4
  #ifdef MAP_POPULATE
    } else if (enif_is_identical(head, ATOM_POPULATE)) {
      f |= MAP_POPULATE;
  #endif
#endif
#ifndef __OSX__
    } else if (enif_is_identical(head, ATOM_SHARED_VALIDATE)) {
      f |= MAP_SHARED_VALIDATE;
#endif
    } else if (enif_is_identical(head, ATOM_SHARED)) {
      f |= MAP_SHARED;
    } else if (enif_is_identical(head, ATOM_ANON)) {
      f |= MAP_ANONYMOUS;
#ifndef __OSX__
    } else if (enif_is_identical(head, ATOM_SYNC)) {
      f |= MAP_SYNC;
#endif
    } else if (enif_is_identical(head, ATOM_FIXED)) {
      f |= MAP_FIXED;
    } else if (enif_is_identical(head, ATOM_FIXED_SIZE)) {
      *fixed_size = true;
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
      } else if (enif_is_identical (tuple[0], ATOM_CHMOD) &&
                 enif_get_long(env, tuple[1], mode)) {
        continue;
      } else if (enif_is_identical (tuple[0], ATOM_MAX_INC_SIZE) &&
                 enif_get_ulong(env,tuple[1], &tmp)) {
        if (tmp > 0)  // Otherwise use default size
          *max_inc_size = tmp;
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
  bool lock, direct, auto_unlink, dbg, fixed_size;
  unsigned long int len;
  unsigned long int offset;
  size_t address, max_inc_size;
  char   path[1024];

  static_assert(sizeof(long int) == sizeof(size_t), "Architecture not supported");

  if (argc != 4
      || !enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1)
      || !enif_get_ulong(env, argv[1], &offset)
      || !enif_get_ulong(env, argv[2], &len)
      || !decode_flags(env, argv[3], &prot, &flags, &open_flags, &mode,
                       &direct, &lock, &auto_unlink,
                       &address, &dbg, &max_inc_size, &fixed_size))
    return enif_make_badarg(env);

  int fd = -1;

  bool exists = false;

  if ((flags & MAP_ANON) == 0) {
    exists = access(path, F_OK) == 0;

    if (!exists) {
      if (len == 0)
        return make_error_tuple(env, "Missing {size, N} option");
      if ((open_flags & O_CREAT) == 0)
        return make_error_tuple(env, "Missing 'create' option");
    }

    fd = open(path, open_flags, (mode_t)mode);

    if (fd < 0) {
      debug(dbg, "open: %s\r\n", strerror_compat(errno));
      return make_error_tuple(env, errno);
    }

    off_t fsize = 0;
    struct stat st;
    if (fstat(fd, &st) < 0) {
      debug(dbg, "fstat: %s\r\n", strerror_compat(errno));
      int err = errno;
      close(fd);
      return make_error_tuple(env, err);
    } else {
      fsize = st.st_size;
    }

    if (exists && len > 0 && fsize != long(len) && fsize > 0) {
      char buf[1280];
      snprintf(buf, sizeof(buf), "File %s has different size (%ld) than requested (%ld)",
               path, fsize, len);
      close(fd);
      return make_error_tuple(env, buf);
    }

    // Stretch the file size to the requested size
    if ((!exists && ((open_flags & (O_CREAT|O_TRUNC)) > 0)) ||
         (exists && fsize == 0)) {
      bool is_ok = ftruncate(fd, 0)   == 0 &&
                   ftruncate(fd, len) == 0;
      if (is_ok)
        debug(dbg, "File %s resized to %d bytes\r\n", path, len);
      else {
        debug(dbg, "ftruncate: %s\r\n", strerror_compat(errno));
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
  auto  size = (size_t)len + offset - pa_offset;
  void* res  = mmap((void*)address, size, prot, flags, fd, (size_t)pa_offset);
  if   (res == MAP_FAILED)
    return make_error_tuple(env, errno);

  if (fd >= 0)
    close(fd);

  auto handle = (mhandle*)enif_alloc_resource(MMAP_RESOURCE, sizeof(mhandle));

  if (!handle) {
    int err = errno;
    munmap(res, size);
    char buf[256];
    snprintf(buf, sizeof(buf), "Cannot allocate resource: %s\n", strerror_compat(err));
    return make_error_tuple(env, buf);
  }

  new (handle) mhandle(res, path, size, direct, fixed_size, max_inc_size, auto_unlink, prot, lock, dbg);
  ERL_NIF_TERM resource = enif_make_resource(env, handle);
  enif_release_resource(handle);

  ERL_NIF_TERM keys[] = {ATOM_EXIST, ATOM_SIZE};
  ERL_NIF_TERM vals[] = {exists ? ATOM_TRUE : ATOM_FALSE, enif_make_ulong(env, size)};

  ERL_NIF_TERM map;
  if (!enif_make_map_from_arrays(env, keys, vals, 2, &map))
    return make_error_tuple(env, ATOM_CANNOT_CREATE_MAP);

  return enif_make_tuple3(env, ATOM_OK, resource, map);
}

static ERL_NIF_TERM emmap_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle* handle;
  if (argc!=1 || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle))
    return enif_make_badarg(env);

  int res;
  RW_LOCK;
  res = handle->unmap(false);
  RW_UNLOCK;

  if (handle->auto_unlink)
    res = unlink(handle->path);

  return res == 0 ? ATOM_OK : make_error_tuple(env, errno);
}

static ERL_NIF_TERM emmap_pread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long pos, bytes;
  mhandle* handle;
  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong(env, argv[1], &pos)
      || !enif_get_ulong(env, argv[2], &bytes)
      || pos >= handle->len
     )
    return enif_make_badarg(env);

  // Adjust bytes to behave like original file:pread/3
  if (pos + bytes > handle->len) bytes = handle->len - pos;

  ErlNifBinary bin;

  if ((handle->prot & PROT_READ) == 0)
    return make_error(env, ATOM_EACCES);

  // if this mmap is direct, use a resource binary
  if (handle->direct) {
    ERL_NIF_TERM res = enif_make_resource_binary
      (env, handle, (void*) (((char*)handle->mem) + pos), bytes);

    return enif_make_tuple2(env, ATOM_OK, res);
  }

  // When it is non-direct, we have to allocate the binary
  if (!enif_alloc_binary((size_t) bytes, &bin))
    return make_error(env, ATOM_ENOMEM);

  R_LOCK;
  if (handle->closed()) {
    R_UNLOCK;
    return make_error(env, ATOM_CLOSED);
  }
  memcpy(bin.data, (void*) (((char*)handle->mem) + pos), bytes);
  R_UNLOCK;

  ERL_NIF_TERM res = enif_make_binary(env, &bin);
  return enif_make_tuple2(env, ATOM_OK, res);
}

static ERL_NIF_TERM emmap_resize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long new_size;
  mhandle* handle;
  if (argc!=2
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong(env, argv[1], &new_size)
     )
    return enif_make_badarg(env);

  if (handle->fixed_size)
    return make_error(env, ATOM_FIXED_SIZE);
#ifdef __OSX__
  return make_error(env, ATOM_FIXED_SIZE);
#else
  if ((handle->prot & PROT_WRITE) == 0) {
    return make_error(env, ATOM_EACCES);
  }

  RW_LOCK;
  if (handle->closed()) {
    RW_UNLOCK;
    return make_error(env, ATOM_CLOSED);
  }

  if (new_size == 0)
    new_size = handle->len < handle->max_inc_size
             ? handle->len * 2 : handle->len + handle->max_inc_size;

  void* addr = mremap(handle->mem, handle->len, new_size, MREMAP_MAYMOVE);
  if (addr == (void*)-1) {
    const char* err = strerror_compat(errno);
    RW_UNLOCK;
    return make_error_tuple(env, err);
  }

  handle->mem = addr;
  handle->len = new_size;
  RW_UNLOCK;

  return enif_make_tuple2(env, ATOM_OK, enif_make_ulong(env, new_size));
#endif
}

static ERL_NIF_TERM emmap_pwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  unsigned long pos;
  mhandle* handle;
  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong(env, argv[1], &pos)
      || !enif_inspect_binary(env, argv[2], &bin)
      || (pos + bin.size) > handle->len
      )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0) {
    return make_error(env, ATOM_EACCES);
  }

  RW_LOCK;
  if (handle->closed()) {
    RW_UNLOCK;
    return make_error(env, ATOM_CLOSED);
  }

  memcpy((void*) (((char*)handle->mem) + pos), bin.data, bin.size);
  RW_UNLOCK;

  return ATOM_OK;
}

/// Atomically read a 64-bit value from the memoty at given position
/// Args:   Handle, Position::integer()
/// Return: Value::integer()
static ERL_NIF_TERM emmap_patomic_read_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;

  if (!(argc==2
        && enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
        && enif_get_ulong   (env, argv[1], &pos)
        && (pos + 8) <= handle->len
     ))
    return enif_make_badarg(env);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);


  void* mem = (char*)handle->mem + pos;
  auto  p   = static_cast<std::atomic<int64_t>*>(mem);
  long  res = p->load(std::memory_order_relaxed);

  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically write a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Value::integer()
static ERL_NIF_TERM emmap_patomic_write_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  ((std::atomic<int64_t>*)mem)->store(value);

  return ATOM_OK;
}

/// Atomically add a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Increment::integer()
/// Return: OldValue::integer()
static ERL_NIF_TERM emmap_patomic_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong   (env, argv[1], &pos)
      || !enif_get_long    (env, argv[2], &value)
      || (pos + 8) > handle->len
     )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error(env, ATOM_EACCES);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  long  res = ((std::atomic<int64_t>*)mem)->fetch_add(value, std::memory_order_relaxed);
  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically subtract a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Decrement::integer()
/// Return: OldValue::integer()
static ERL_NIF_TERM emmap_patomic_sub(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong   (env, argv[1], &pos)
      || !enif_get_long    (env, argv[2], &value)
      || (pos + 8) > handle->len
     )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error(env, ATOM_EACCES);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  long  res = ((std::atomic<int64_t>*)mem)->fetch_sub(value, std::memory_order_relaxed);
  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically AND a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Value::integer()
/// Return: ResValue::integer()
static ERL_NIF_TERM emmap_patomic_and(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong   (env, argv[1], &pos)
      || !enif_get_long    (env, argv[2], &value)
      || (pos + 8) > handle->len
     )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error(env, ATOM_EACCES);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  long  res = ((std::atomic<int64_t>*)mem)->fetch_and(value, std::memory_order_relaxed);
  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically OR a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Value::integer()
/// Return: ResValue::integer()
static ERL_NIF_TERM emmap_patomic_or(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong   (env, argv[1], &pos)
      || !enif_get_long    (env, argv[2], &value)
      || (pos + 8) > handle->len
     )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error(env, ATOM_EACCES);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  long  res = ((std::atomic<int64_t>*)mem)->fetch_or(value, std::memory_order_relaxed);
  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically XOR a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Value::integer()
/// Return: ResValue::integer()
static ERL_NIF_TERM emmap_patomic_xor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong   (env, argv[1], &pos)
      || !enif_get_long    (env, argv[2], &value)
      || (pos + 8) > handle->len
     )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error(env, ATOM_EACCES);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  long  res = ((std::atomic<int64_t>*)mem)->fetch_xor(value, std::memory_order_relaxed);
  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically exchange a 64-bit value to the memoty at given position
/// Args:   Handle, Position::integer(), Value::integer()
/// Return: ResValue::integer()
static ERL_NIF_TERM emmap_patomic_xchg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          value;

  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong   (env, argv[1], &pos)
      || !enif_get_long    (env, argv[2], &value)
      || (pos + 8) > handle->len
     )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error(env, ATOM_EACCES);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  long  res = ((std::atomic<int64_t>*)mem)->exchange(value, std::memory_order_acq_rel);
  return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, res));
}

/// Atomically compare and swap a 64-bit value to the memoty at given position
/// with the new_val if the value is equal to the `old_val`.
/// If the memory at given `pos` is not aligned to long, return `{error, alignment}`.
/// If CAS fails, return `{false, OldVal}`, where OldVal is the value currently read at `pos`.
/// Otherwise return `{true, OldVal}`.
static ERL_NIF_TERM emmap_patomic_cas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle*      handle;
  unsigned long pos;
  long          old_val, new_val;

  if (argc!=4
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_ulong   (env, argv[1], &pos)
      || !enif_get_long    (env, argv[2], &old_val)
      || !enif_get_long    (env, argv[3], &new_val)
      || (pos + 8) > handle->len
     )
    return enif_make_badarg(env);

  if ((handle->prot & PROT_WRITE) == 0)
    return make_error(env, ATOM_EACCES);

  if (handle->closed())
    return make_error(env, ATOM_CLOSED);

  void* mem = (char*)handle->mem + pos;
  size_t sz = handle->len - pos;

  if (!std::align(sizeof(long), sizeof(long), mem, sz))
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_ALIGNMENT);

  auto p   = reinterpret_cast<std::atomic<long>*>(mem);
  auto res = p->compare_exchange_weak(old_val, new_val, std::memory_order_release,
                                      std::memory_order_relaxed);
  return enif_make_tuple2(env, res ? ATOM_TRUE : ATOM_FALSE, enif_make_long(env, old_val));
}

static ERL_NIF_TERM emmap_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle* handle;
  unsigned long bytes;

  if (!enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle) ||
      !enif_get_ulong(env, argv[1], &bytes))
    return enif_make_badarg(env);

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
    if (!enif_alloc_binary((size_t) size, &bin))
      return make_error(env, ATOM_ENOMEM);

    memcpy(bin.data, (void*) (((char*)handle->mem) + start), size);

    ERL_NIF_TERM res = enif_make_binary(env, &bin);
    return enif_make_tuple2(env, ATOM_OK, res);
  }
}

static ERL_NIF_TERM emmap_read_line(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle* handle;

  if (!enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle))
    return enif_make_badarg(env);

  RW_LOCK;

  if (handle->position == handle->len) {
    RW_UNLOCK;
    return ATOM_EOF;
  }

  long start = handle->position;
  char* current = ((char*)handle->mem) + handle->position;
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
  if (not got_eof)
    handle->position++;

  no_crlf_len = linelen = handle->position - start;

  if (not got_eof) {
    // Found LF -- exclude it from line
    no_crlf_len--;
    // If line length before \n is non-zero check if we have \r before \n
    if ((no_crlf_len > 0) && (*(current - 1) == '\r')) {
      have_cr = true;
      no_crlf_len--;
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
      return make_error(env, ATOM_ENOMEM);
    }

    memcpy(bin.data, (void*) (((char*)handle->mem) + start), no_crlf_len);
    // Set trailing \n if needed
    if (not got_eof) *(((char*)bin.data) + no_crlf_len) = '\n';

    ERL_NIF_TERM res = enif_make_binary(env, &bin);
    return enif_make_tuple2(env, ATOM_OK, res);
  }
}

static ERL_NIF_TERM emmap_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mhandle* handle;
  long position;
  long relpos;
  if (argc!=3
      || !enif_get_resource(env, argv[0], MMAP_RESOURCE, (void**)&handle)
      || !enif_get_long(env, argv[2], &relpos)
      || (argv[1] != ATOM_CUR && argv[1] != ATOM_BOF && argv[1] != ATOM_EOF))
    return enif_make_badarg(env);

  RW_LOCK;

  if (argv[1] == ATOM_BOF)
    position = 0L + relpos;
  else if (argv[1] == ATOM_CUR)
    position = handle->position + relpos;
  else if (argv[1] == ATOM_EOF)
    position = handle->len - relpos;
  else
    position = -1;

  if (position < 0L || ((unsigned long)position) > handle->len) {
    RW_UNLOCK;
    return enif_make_badarg(env);
  }

  handle->position = position;
  RW_UNLOCK;

  return enif_make_tuple2(env, ATOM_OK, enif_make_ulong(env, position));
}
