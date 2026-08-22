# Enroll this machine as an age recipient for the repo's sops secrets.
#
# Generates a machine-local age key if absent, adds its public half to
# .sops.yaml, and re-encrypts secrets to the new recipient list. The
# re-encryption decrypts with the OpenPGP key first, so a YubiKey must be
# inserted and you will be asked for its PIN once.
#
# Run from anywhere inside the dotfiles repo:  nix run .#enroll-host

KEY_FILE="${SOPS_AGE_KEY_FILE:-$HOME/.config/sops/age/keys.txt}"

REPO="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [ -z "$REPO" ]; then
  echo "error: not inside a git repo - run this from the dotfiles checkout" >&2
  exit 1
fi
cd "$REPO"

SOPS_YAML="$REPO/.sops.yaml"
SECRETS="$REPO/secrets/secrets.yaml"
for f in "$SOPS_YAML" "$SECRETS"; do
  [ -f "$f" ] || { echo "error: missing $f" >&2; exit 1; }
done

# Fresh machines have no card stubs in ~/.gnupg until gpg has seen the card.
if ! gpg --card-status >/dev/null 2>&1; then
  echo "warning: no OpenPGP card detected - re-encryption will fail without one" >&2
fi

if [ ! -f "$KEY_FILE" ]; then
  echo "generating machine age key at $KEY_FILE"
  mkdir -p "$(dirname "$KEY_FILE")"
  age-keygen -o "$KEY_FILE"
  chmod 600 "$KEY_FILE"
fi

PUB="$(age-keygen -y "$KEY_FILE")"
HOST="$(hostname -s)"

if grep -qF "$PUB" "$SOPS_YAML"; then
  echo "$HOST is already enrolled ($PUB)"
  exit 0
fi

echo "enrolling $HOST -> $PUB"

python3 - "$SOPS_YAML" "$PUB" "$HOST" <<'PYEOF'
import re, sys

path, pub, host = sys.argv[1], sys.argv[2], sys.argv[3]
lines = open(path).read().splitlines()


def find_key(name):
    """Locate a mapping key, allowing for it being the first key of a YAML
    list item (`- pgp:`). Returns (line index, indent of the key itself)."""
    pat = re.compile(r"^(\s*(?:-\s+)?)" + name + r":\s*$")
    for i, line in enumerate(lines):
        m = pat.match(line)
        if m:
            return i, " " * len(m.group(1))
    return None, None


age_idx, age_indent = find_key("age")

if age_idx is not None:
    lines.insert(age_idx + 1, f"{age_indent}  - {pub} # {host}")
else:
    pgp_idx, pgp_indent = find_key("pgp")
    if pgp_idx is None:
        sys.exit("error: no pgp: block found in .sops.yaml")

    # Walk past the pgp entries: stop at the first non-blank line that is not
    # indented deeper than the 'pgp:' key itself.
    j = pgp_idx + 1
    while j < len(lines):
        s = lines[j]
        if not s.strip():
            j += 1
            continue
        if len(s) - len(s.lstrip()) <= len(pgp_indent):
            break
        j += 1

    lines[j:j] = [f"{pgp_indent}age:", f"{pgp_indent}  - {pub} # {host}"]

open(path, "w").write("\n".join(lines) + "\n")
PYEOF

echo "re-encrypting secrets to the new recipient list (YubiKey PIN required)..."
sops updatekeys --yes "$SECRETS"

git add "$SOPS_YAML" "$SECRETS"
git commit -m "Enroll $HOST as sops age recipient"

echo
echo "done. $HOST can now decrypt unattended once this is pushed and rebuilt:"
echo "    git push"
echo "    sudo darwin-rebuild switch --flake $REPO   # or nixos-rebuild"
