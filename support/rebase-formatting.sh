#!/bin/bash

# rebase-formatting.sh
#
# The whole point of this script is to distribute formatting changes evenly
# across your branch, without frivolous merge conflicts. It does this by
# carefully managing:
# - reverting formatting changes
# - applying your changes
# - reformatting your changes
# - folding the revert and reformat commits back into the original commit
#
# This script assumes a lot, but I've done effectively this same approach
# manually for years, so it seemed like a good idea to actually write it
# down.
#
# Assumptions:
# - You _must_ have a clean worktree, at least as far as modules are
#   concerned. This script does `git reset --hard` during its operation.
# - The two arguments passed in are _commit hashes_, not branch names.
#   This is to permit you to easily recover from any mistakes this script
#   makes.
# - You execute the script from a detached head state, again to permit easy
#   recovery from mistakes in the script.
# - You are running a reasonably recent version of BASH.
#   This was written against 4.4.23 on OSX.
# - If using sbt-client, the version of the scalafmt plugin loaded into your
#   sbt driver agrees with the version specified in `format_hash`
# - You _must_ have your branch up-to-date with the commit immediately before
#   `format_hash` for the greatest chance of success.
# - You _must not_ have upgraded to the latest scalafmt and reformatted the
#   codebase already. If you've done that, you can run this script from the
#   detached head immediately before doing the format, then likely simply
#   rebase the rest of your branch ontop of whatever this script leaves you
#   with at the end.
#
# Other oddities:
# - If you have a formatting-only commit, you may be dropped into a shell with
#   a recent message being effectively "Nothing to do!". Just type
#   `git rebase --continue` to proceed.


# Find a useful sbt runner. sbt-client makes this whole operation much faster,
# as running scalafmt in a warm sbt session only takes a few seconds, vs 20+
# seconds on my machine starting cold.
if hash sbt-client 2>/dev/null; then
  sbt="sbt-client"
elif hash sbt 2>/dev/null; then
  sbt="sbt"
else
  echo "Unable to find a suitable sbt"
  exit 1
fi

format_cmd="${sbt} scalafmt"
for project in akkaHttpSample codegen dropwizardSample endpointsSample http4sSample microsite; do
  format_cmd="${format_cmd} && ${sbt} ${project}/scalafmt && ${sbt} ${project}/test:scalafmt"
done


# We use this marker to indicate which commits we should act on. It's somewhat
# unlikely that it'll conflict. Tested against BSD and GNU `base64`
marker="$(echo "${RANDOM}" | base64)"

if [[ "${#@}" -eq 2 ]]; then
# common_root is the SHA of the commit immediately before the formatting commit.
# This is expected to be the closest ancestor to your branch, as well as the
# formatting commit.
  common_root="$1"
# format_hash is the SHA of the formatting commit. This is the first one to
# be reverted, as mentioned at the top of this script.
  format_hash="$2"
elif [[ "${#@}" -eq 0 ]]; then
  echo "Assuming hard-coded default values for common_root and format_hash" >&2
  echo "Hopefully this is what you want. If so, press enter." >&2
  read -p 'Press enter to proceed with defaults: '
  format_hash="b1cd9fa287df65246523b13c88cb808537075c72"
  common_root="$(git rev-parse "${format_hash}^")"
else
  echo "Usage: ${0} [<common_root> <format_hash>]" >&2
  exit 2
fi

# Base assumption is that this'll work at all.
# Otherwise, it'll raise some hopefully minor merge conflicts to the user
# before getting started.
git rebase -q "${common_root}"

# Keep track of where we started, so we can apply the whole range of the
# changeset ontop of the reverted formatting commit.
current_hash="$(git rev-parse @)"

# Revert the formatting commit
git checkout -q "${format_hash}"
git revert --no-edit -n "${format_hash}"

# Leave an indicator to future commands that we need to fold this revert
# commit into the next commit.
git commit -m "${marker} @v reverting"

# Walk the whole changeset, interpolating in reformat && revert to avoid
# merge conflicts
git rebase -i "${common_root}" "${current_hash}" --onto @ \
  -x "set -x; ${format_cmd} && git commit -m '${marker} @^ formatting' modules && git revert --no-edit -n @ && git commit -m '${marker} @v reverting'; set +x"

# The very last commit may include just a lone revert of formatting changes.
# It must be stopped.
if [[ "$(git log --oneline --format=%s -n 1)" = "${marker} @v reverting" ]]; then
  git reset --hard @^
fi

# Finally, one last walk to fold the format/revert commits into their
# associated commit.
# - The first one effectively does a FIXDOWN of the revert commit into the
#   current commit
# - The second one does a long-form FIXUP of the formatting changes, again
#   into the current commit
#
# By bracketing each change like this, the commit only ever attempts to be
# applied against the original formatting, and since we keep reformatting
# with the same rules (provided formatting is deterministic), we can
# seamlessly move towards a better formatted world.
git rebase "${format_hash}" \
  -x "set -x; [[ \"\$(git log --oneline --format=%s -n 1)\" = \"${marker}\"*              ]] || (git reset @^^; git add modules; git commit         -C @@{1} modules) || true; set +x" \
  -x "set -x; [[ \"\$(git log --oneline --format=%s -n 1)\" = \"${marker} @^ formatting\" ]] && (git reset @^ ; git add modules; git commit --amend -C @     modules) || true; set +x"
