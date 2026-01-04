#ifndef CRITERIUM_UTILS_H
#define CRITERIUM_UTILS_H

#include <algorithm>
#include <array>
#include <cstring>

namespace criterium {

struct PrefixEntry {
  const char* prefix;
  size_t length;
};

constexpr std::array<PrefixEntry, 6> SYSTEM_PREFIXES{{
  {"Ljava/", 6},
  {"Lcom/sun/", 9},
  {"Ljdk/", 5},
  {"Ljavax/", 7},
  {"Lsun/management", 15},
  {"Lclojure/", 9}
}};

inline bool is_system_class(const char* class_name) {
  return std::any_of(SYSTEM_PREFIXES.begin(), SYSTEM_PREFIXES.end(),
    [class_name](const PrefixEntry& entry) {
      return strncmp(class_name, entry.prefix, entry.length) == 0;
    });
}

} // namespace criterium

#endif // CRITERIUM_UTILS_H
