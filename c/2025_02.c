#if 0
cc -Wall -Wextra -g $0
./a.out $@
exit
#endif
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "aoc.h"

Pair(uint64_t, const char*)
parse_u64(const char* str, const char* end) {
   uint64_t n = 0;
   while (str != end) {
      auto c = *str - '0';
      if (c < 0 || c > 9) break;
      n = (n * 10) + c;
      ++str;
   }
   return (__typeof(parse_u64(str, end))){n, str};
}

void
one(const char* str, const char* end) {
   uint64_t sum = 0;
   while (str != end) {
      auto n_x_str = parse_u64(str, end);
      auto n = n_x_str._0;
      auto m_x_str = parse_u64(n_x_str._1 + 1, end);
      auto m = m_x_str._0;
      str = m_x_str._1 + 1;

      for (auto i = n; i <= m; ++i) {
         auto digits = (unsigned)__builtin_log10(i) + 1;
         if (digits % 2 == 0) {
            auto d = (uint64_t)__builtin_exp10(digits / 2);
            if (i / d == i % d) sum += i;
         }
      }
   }
   printf("%llu\n", (unsigned long long)sum);
}

void
two(const char* str, const char* end) {
   uint64_t sum = 0;
   while (str != end) {
      auto n_x_str = parse_u64(str, end);
      auto n = n_x_str._0;
      auto m_x_str = parse_u64(n_x_str._1 + 1, end);
      auto m = m_x_str._0;
      str = m_x_str._1 + 1;

      for (auto i = n; i <= m; ++i) {
         auto digits = (unsigned)__builtin_log10(i) + 1;
         for (unsigned j = 0; j <= digits / 2; ++j) {
            if (j != 1 && digits % j != 0) {
               continue;
            }
            auto d = (uint64_t)__builtin_exp10(j);
            auto e = d;
            for (unsigned k = 0; k < (digits / j) - 1; ++k) {
               if ((i / e) % d != i % d) goto out;
               e *= d;
            }
            sum += i;
            break;
out:
            (void)0;
         }
      }
   }
   printf("%llu\n", (unsigned long long)sum);
}

int
main(int argc, char **argv) {
   if (argc != 2) goto fail;
   auto str = slurp(argv[1]);
   if (!str._0) goto fail;

   one(str._0, str._0 + str._1);
   two(str._0, str._0 + str._1);
   return 0;
fail:
   fprintf(stderr, "invalid arguments\n");
   return -1;
}
