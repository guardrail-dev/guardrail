#!/bin/env bash

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

curl --silent -o "${github_api}" "https://api.github.com/repos/twilio/guardrail/compare/${last_tag}...master"

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
