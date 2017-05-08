
# Until someone figures out how to use Cabal to pluck out this string
cat -- "$1" | grep '^version' | awk -F: '{ print $2 }' | sed -E 's/^[[:space:]]+//'

