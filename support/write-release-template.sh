#!/bin/env bash

for remote in github upstream origin; do
        git remote | grep "$remote" && break 2>/dev/null >&2
        unset remote
done
if [ ! -z "$remote" ]; then
        git fetch -p "$remote"
else
        echo "Unable to find workable remote, not fetching." >&2
fi

last_tag="$(git tag | tail -n1)"
read -p "Last release (${last_tag}): " new_last_tag
if [ ! -z "${new_last_tag}" ]; then
        last_tag="${new_last_tag}"
        unset new_last_tag
fi

new_tag="$1"

if [ -z "${new_tag}" ]; then
        read -p 'Tag: ' new_tag
        if [ "${new_tag}" = "${new_tag#v}" ]; then
                new_tag="v${new_tag}"
        fi
fi
notes_file="notes/${new_tag#v}.md"

if [ -e "$notes_file" ]; then
        echo "Error: ${notes_file} already exists"
        exit 1
fi


github_api="$(mktemp)"

current_head="$(git rev-parse @)"

if [ ! -z "$remote" ]; then
        newer_prs="$(git log --merges --oneline "${current_head}...${remote}/master" | grep 'Merge pull request' | wc -l)"
        if [ "$newer_prs" -gt 0 ]; then
                echo "Preparing to release old version, not including $newer_prs PRs" >&2
        fi
fi

curl --silent -o "${github_api}" "https://api.github.com/repos/twilio/guardrail/compare/${last_tag}...${current_head}"

cat > "${notes_file}" <<!
TITLE
====

Included issues:
!

jq -r '.commits | map(select(.parents | length == 2)) | map(.commit.message | sub("Merge pull request (?<a>#\\d*) .*\n\n"; "- twilio/guardrail" + .a + " "))[]' < "${github_api}" >> "${notes_file}"

cat >> "${notes_file}" <<!

Contributors:
!

jq -r '.commits[].author.login' < "${github_api}" | sort | uniq -c | sort -rn | sed 's/^ *[[:digit:]]\+ /- @/' >> "${notes_file}"

echo "Successfully wrote ${notes_file}"
