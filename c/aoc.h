#ifndef AOC_H
#define AOC_H
#include <fcntl.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#define auto __auto_type

#define cat_(x, y) x##y
#define cat(x, y) cat_(x, y)

#define xchg_(n, l, r) __extension__ ({ \
   auto cat(sym, n) = l; \
   auto cat(sym1, n) = *cat(sym, n); \
   *cat(sym, n) = r; \
   cat(sym1, n); \
})
#define xchg(l, r) xchg_(__COUNTER__, l, r)

#define try_(n, ...) __extension__ ({ \
   auto cat(sym, n) = __VA_ARGS__; \
   if ( \
      __builtin_choose_expr( \
         (__builtin_classify_type(cat(sym, n)) ^ 4) <= 1, \
         !cat(sym, n), \
         cat(sym, n) < 0)) \
      goto fail; \
   cat(sym, n); \
})
#define try(...) try_(__COUNTER__, __VA_ARGS__)

#define Pair(T, U) struct { T _0; U _1; }

#define mod_(n, a, b) __extension__ ({ \
   auto cat(sym, n) = a; \
   auto cat(sym1, n) = b; \
   auto cat(sym2, n) = cat(sym, n) % cat(sym1, n); \
   if (cat(sym2, n) < 0) \
      cat(sym2, n) += cat(sym1, n) < 0 ? -cat(sym1, n) : cat(sym1, n); \
   cat(sym2, n); \
})
#define mod(a, b) mod_(__COUNTER__, a, b)

static inline void *
xmalloc(size_t len) {
   auto p = malloc(len);
   if (!p) abort();
   return p;
}

static inline void *
xrealloc(void *p, size_t len) {
   p = realloc(p, len);
   if (!p) abort();
   return p;
}

static Pair(const char *, size_t)
slurp(const char *fname) {
   char *buf = NULL;
   size_t cap = 0;
   auto fd = -1;
   fd = try (open(fname, O_RDONLY));
   cap = try (lseek(fd, 0, SEEK_END)) + 1;
   try(lseek(fd, 0, SEEK_SET));
   buf = xmalloc(cap);

   for (size_t len = 0;;) {
      size_t len1 = try (read(fd, buf, cap - len));
      if (len1 == 0) {
          close(fd);
          return (__typeof(slurp(fname))){buf, len};
      }

      len += len1;
      if (len == cap) {
         try ((bool)!__builtin_umull_overflow(cap, 3, &cap));
         buf = xrealloc(buf, (cap /= 2));
      }
   }
fail:
   if (fd >= 0) close(fd);
   free(buf);
   return (__typeof(slurp(fname))){NULL, 0};
}
#endif
