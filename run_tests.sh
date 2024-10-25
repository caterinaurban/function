#!/bin/bash

shopt -s extglob

execname="./main.exe"
in_file="tests.ref"
tab=$'\t'

tottests=0
totf=0
nsucc=0
nokf=0

# Colors
redcolor=$'\e[1;31m'
lightredcolor=$'\e[91m'
lightgreencolor=$'\e[92m'
greycolor=$'\e[2m'
resetcolor=$'\e[m'

progress() {
	if (( $1 == 0 ))
	then
		printf $'\e[G\e[K' # WARNING: Erase in Line
	elif (( $1 == 1 ))
	then
		printf $'\e[GPhase 1: scanning file %d, test %d...' $totf $tottests
	else
		printf $'\e[GPhase %d: file %'"${#totf}"$'d/%d, test %'"${#tottests}"$'d/%d (%d %%)... (Time remaining: %02d:%02d)' $1 $2 $totf $3 $tottests $(( $3 * 100 / tottests )) $(( $4 / 60 )) $(( $4 % 60 ))
	fi
}

# First pass: count the number of tests etc
maximum=0
while IFS= read -r line
do
	f="${line%%	*}"
	[[ -z "$f" ]] && continue
	[[ -f "$f" ]] || continue
	
	: $(( ++totf ))
	ntests=0
	
	line="${line/#+([^	])	}"
	while [[ "$line" =~ ^[^$tab]+"$tab"[[:digit:]]+"$tab"[^$tab]*"$tab" ]]
	do
		: $(( ++ntests ))
		timeout="${line%%	*}"
		line="${line##+([^	])	+([^	])	*([^	])	*([^	])?(	)}"
		if [[ "$timeout" =~ ^[0-9]+s$ ]]
		then
			: $(( maximum += ${timeout%s} ))
		elif [[ "$timeout" =~ ^[0-9]+m$ ]]
		then
			: $(( maximum += 60 * ${timeout%m} ))
		else
			progress 0
			echo "${redcolor}Error in reference:${resetcolor} file '$f' has invalid or unknown timeout value: {$timeout}"
		fi
	done
	if [[ ! -z "$line" ]]
	then
		progress 0
		echo "${redcolor}Error in reference:${resetcolor} file '$f' has trailing text on the line: {$line}"
	fi
	
	if [[ $ntests -eq 0 ]]
	then
		progress 0
		echo "${redcolor}Error in reference:${resetcolor} file '$f' has no test"
	fi
	: $(( tottests += ntests ))
	progress 1
done <"$in_file"

ntest=0
nf=0
while IFS= read -r line
do
	f="${line%%	*}"
	[[ -z "$f" ]] && continue
	[[ -f "$f" ]] || continue
	
	: $(( ++nf ))
	ntests=0
	noks=0
	errs=""
	
	line="${line/#+([^	])	}"
	while [[ "$line" =~ ^[^$tab]+"$tab"[[:digit:]]+"$tab"[^$tab]*"$tab" ]]
	do
		progress 2 $nf $(( ++ntest )) $maximum
		: $(( ++ntests ))
		timeout="${line%%	*}"
		line="${line##+([^	])	}"
		expcode="${line%%	*}"
		line="${line##+([^	])	}"
		expout="${line%%	*}"
		line="${line##*([^	])	}"
		params="${line%%	*}"
		line="${line##*([^	])?(	)}"
		
		output=$(timeout --foreground "$timeout" "$execname" $params "$f" 2>&1)
		errcode=$?
		
		if (( errcode != expcode ))
		then
			errs="$errs${greycolor}[${resetcolor}$params${greycolor}] ${resetcolor}${lightredcolor}Unexpected return code:${resetcolor}${greycolor} expected ${resetcolor}${lightgreencolor}$expcode${resetcolor}${greycolor}"
			if (( expcode == 0 ))
			then
				errs="$errs ($expout)"
			fi
			errs="$errs, got ${resetcolor}${lightredcolor}$errcode${resetcolor}"$'\n'
		elif (( errcode == 0 )) && [[ $(echo -- "$output" | grep "Analysis Result: " | cut -d' ' -s -f3-) != "$expout" ]]
		then
			errs="$errs${greycolor}[${resetcolor}$params${greycolor}] ${resetcolor}${lightredcolor}Unexpected return message:${resetcolor}${greycolor} expected ${resetcolor}${lightgreencolor}$expout${resetcolor}${greycolor}, got ${resetcolor}${lightredcolor}$(echo -- "$output" | grep "Analysis Result: " | cut -d' ' -s -f3-)${resetcolor}"$'\n'
		else
			: $(( ++noks ))
		fi
		
		if [[ "$timeout" =~ ^[0-9]+s$ ]]
		then
			: $(( maximum -= ${timeout%s} ))
		elif [[ "$timeout" =~ ^[0-9]+m$ ]]
		then
			: $(( maximum -= 60 * ${timeout%m} ))
		else
			progress 0
			echo "${redcolor}Error in reference:${resetcolor} file '$f' has invalid or unknown timeout value: {$timeout}"
		fi
	done
	if [[ ! -z "$errs" ]]
	then
		progress 0
		echo "${redcolor}Error on $f:${resetcolor}"$'\n'"$errs"
	fi
	
	: $(( nsucc += noks ))
	[[ $noks -eq $ntests ]] && : $(( ++nokf )) || true
done <"$in_file"

progress 0
if [[ $nsucc -eq $tottests ]]
then
	echo "Result:"
	echo "${greycolor}All tests (${resetcolor}${lightgreencolor}$tottests${resetcolor}${greycolor} over ${resetcolor}${lightgreencolor}$totf${resetcolor}${greycolor} files) ${resetcolor}${lightgreencolor}passed!${resetcolor}"
else
	echo "Result:"
	echo "${lightredcolor}$nokf${resetcolor}/${lightgreencolor}$totf${resetcolor}${greycolor} files passed (${resetcolor}$(( nokf * 100 / totf ))%${resetcolor}${greycolor})${resetcolor}"
	echo "${greycolor}Passed ${resetcolor}${lightredcolor}$nsucc${resetcolor}/${lightgreencolor}$tottests${resetcolor}${greycolor} (${resetcolor}$(( nsucc * 100 / tottests ))%${greycolor}) tests${resetcolor}"
fi
