#if 0
cc -Wall -Wextra -g $0
./a.out $@
exit
#endif
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include "aoc.h"

Pair(int, const char*)
parse_int(const char* str, const char* end) {
   auto n = 0;
   while (str != end) {
      auto c = *str - '0';
      if (c < 0 || c > 9) break;
      n *= 10;
      n += c;
      ++str;
   }
   return (__typeof(parse_int(str, end))){n, str};
}

void
one(const char* str, const char* end) {
   auto state = 50;
   unsigned zeroes = 0;
   while (str != end) {
      int sign;
      switch (*str++) {
      break;case 'L': sign = -1;
      break;case 'R': sign = 1;
      break;default: assert(false);
      }

      auto n_x_str = parse_int(str, end);
      state = mod(state + (n_x_str._0 * sign), 100);
      if (state == 0) ++zeroes;
      str = n_x_str._1;
      if (str != end) {
         auto c = *str++;
         assert(c == '\n');
      }
   }
   printf("%u\n", zeroes);
}

void
two(const char* str, const char* end) {
   auto state = 50;
   unsigned zeroes = 0;
   while (str != end) {
      int sign;
      switch (*str++) {
      break;case 'L': sign = -1;
      break;case 'R': sign = 1;
      break;default: assert(false);
      }

      auto n_x_str = parse_int(str, end);
      auto state1 = state + (n_x_str._0 * sign);
      if (state1 > 0) zeroes += state1 / 100;
      else {
         zeroes -= state1 / 100;
         if (state != 0) ++zeroes;
      }
      state = mod(state1, 100);

      str = n_x_str._1;
      if (str != end) {
         auto c = *str++;
         assert(c == '\n');
      }
   }
   printf("%u\n", zeroes);
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
