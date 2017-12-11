#!/usr/bin/env bash
# replace normal elm-make with sysconfcpus-prefixed elm-make
# epic build time improvement - see https://github.com/elm-lang/elm-compiler/issues/1473#issuecomment-245704142
# Using `pwd` for somewhat unsafe directory detection; since $HOME is not available in CircleCI
set -euo pipefail
if ! grep "sysconfcpus" "$(npm bin)/elm-make"; then
  mv "$(npm bin)/elm-make" "$(npm bin)/elm-make-old"
  cat << EOF > "$(npm bin)/elm-make"
#!/usr/bin/env bash
set -eu
echo "Running elm-make with sysconfcpus -n 2"
$(pwd)/sysconfcpus/bin/sysconfcpus -n 2 "$(npm bin)/elm-make-old" "\$@"
EOF
  chmod +x "$(npm bin)/elm-make"
fi
