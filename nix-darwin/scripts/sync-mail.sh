# Mail sync: mbsync -> notmuch -> afew, run from mcron every 5 minutes.
# PATH and MAIL_LOG_DIR are exported by the Nix wrapper in kite.nix.

lock_dir="$HOME/.cache/mail-sync.lock"
# Kept outside ~/.mail so that notmuch does not index the dumps as mail.
backup_dir="$HOME/.local/state/notmuch/tag-backups"

groups="jcash-support jcash-ops jcash-compliance jcash-info jcash-fraud
        jcash-hr jcash-sales jcash-system jgroup-ben jgroup-system
        jfund gmail protonmail"

# mcron fires every 5 minutes; skip if the previous run is still going,
# otherwise two mbsync processes race on the same maildirs and state files.
mkdir -p "$HOME/.cache"
if ! mkdir "$lock_dir" 2>/dev/null; then
  if [ -r "$lock_dir/pid" ] && ! kill -0 "$(cat "$lock_dir/pid")" 2>/dev/null; then
    rm -rf "$lock_dir"
    mkdir "$lock_dir" || exit 0
  else
    echo "mail-sync: previous run still active, skipping" >&2
    exit 0
  fi
fi
echo $$ > "$lock_dir/pid"
trap 'rm -rf "$lock_dir"' EXIT

failed=""
synced=0
for group in $groups; do
  err=$(mktemp)
  if mbsync "$group" 2>"$err"; then
    synced=$((synced + 1))
  else
    failed="$failed $group"
  fi
  # The Proton Bridge channels talk cleartext to 127.0.0.1 by design; that
  # warning alone accounted for ~32k lines of the error log.
  grep -v 'Password is being sent in the clear' "$err" >&2 || true
  rm -f "$err"
  sleep 1
done

if [ -n "$failed" ]; then
  echo "mail-sync: FAILED groups:$failed" >&2
fi

if [ "$synced" -eq 0 ]; then
  echo "mail-sync: every group failed, skipping notmuch/afew" >&2
  exit 1
fi

notmuch new
afew -n -t

# Tags live only in the Xapian DB and are not re-downloadable; keep 14.
today=$(date +%F)
mkdir -p "$backup_dir"
if [ ! -f "$backup_dir/tags-$today.dump" ]; then
  notmuch dump --output="$backup_dir/tags-$today.dump"
  ls -1t "$backup_dir"/tags-*.dump | tail -n +15 | while read -r stale; do
    rm -f "$stale"
  done
fi

# launchd holds these open in append mode, so truncate in place rather than
# renaming: a rotated-away inode would keep receiving all future output.
for logf in "$MAIL_LOG_DIR/mcron.err.log" "$MAIL_LOG_DIR/mcron.out.log"; do
  if [ -f "$logf" ] && [ "$(stat -c%s "$logf")" -gt 10485760 ]; then
    tail -c 2097152 "$logf" > "$logf.tmp" && cat "$logf.tmp" > "$logf"
    rm -f "$logf.tmp"
  fi
done

[ -z "$failed" ]
