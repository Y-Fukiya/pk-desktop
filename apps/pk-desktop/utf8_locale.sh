desktop_ensure_utf8_locale() {
  case "$(locale charmap 2>/dev/null)" in
    UTF-8|utf-8|UTF8|utf8) return 0 ;;
  esac
  desktop_locales="$(locale -a 2>/dev/null)" || return 1
  desktop_utf8_locales="$(printf '%s\n' "$desktop_locales" | grep -Ei '\.utf-?8$' || true)"
  for desktop_locale in C.UTF-8 C.utf8 en_US.UTF-8 en_US.utf8 ja_JP.UTF-8 ja_JP.utf8 $desktop_utf8_locales; do
    if printf '%s\n' "$desktop_locales" | grep -Fxq "$desktop_locale"; then
      case "$(LC_ALL="$desktop_locale" locale charmap 2>/dev/null)" in
        UTF-8|utf-8|UTF8|utf8)
          export LC_ALL="$desktop_locale"
          return 0
          ;;
      esac
    fi
  done
  printf '%s\n' 'PK Desktop requires an available UTF-8 locale. Configure LC_ALL or install an OS UTF-8 locale.' >&2
  return 1
}
