#!/usr/bin/bash
main() {
  local -a tools=("dm" "dmc")
  for tool in "${tools[@]}"; do
    curl -L "http://get.iceburg.net/dockmaster/latest-0.1.x/$tool" > /usr/local/bin/$tool && \
      chmod +x /usr/local/bin/$tool
  done
}
main "$@"
