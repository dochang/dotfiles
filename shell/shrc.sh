## If `sh` links to `zsh`, and is invoked as a "non-login, non-interactive"
## shell, this file should be skipped.
if ! { [ x"$(ps c -p $$ -o 'comm=' 2>/dev/null || true)" = xsh -a -n "${ZSH_VERSION}" ] && expr "$-" : '.*i' > /dev/null ; } ; then
	get_alias () {
		local a
		a=$(alias $1 2>/dev/null)
		if [ -z "$a" ]; then
			echo "$1"
		else
			( eval "$a ; echo \$$1" )
		fi
	}

	if [ "$TERM" != "dumb" ]; then
		alias ls='ls --color=auto -F -tr --quoting-style=shell'
		alias dir='ls --color=auto --format=vertical'
		alias vdir='ls --color=auto --format=long'
	else
		alias ls='ls -F -tr'
		alias dir='ls --format=vertical'
		alias vdir='ls --format=long'
	fi

	alias rm='rm -I'
	alias cp='cp -i'
	alias mv='mv -i'

	alias grep="$(get_alias grep) --color=auto"

	is_function () {
		type $1 | grep -E ' is (.* )?function' >/dev/null 2>&1
	}

	## Define `*env` functions if the shell is invoked as non-login.
	which anyenv >/dev/null 2>&1 && ! is_function anyenv && eval "$(anyenv init -)"
	which pyenv >/dev/null 2>&1 && {
		case "$(ps c -p $$ -o 'comm=' 2>/dev/null || true)" in
		bash|zsh|ksh)
			pyenv virtualenvwrapper 2>/dev/null
			;;
		esac
	}

	# https://transfer.sh/
	# https://gist.github.com/nl5887/a511f172d3fb3cd0e42d
	transfer() {
		# check arguments
		if [ $# -eq 0 ];
		then
			echo "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md"
			return 1
		fi

		# get temporarily filename, output is written to this file show progress can be showed
		tmpfile=$( mktemp -t transferXXX )

		# upload stdin or file
		file=$1

		if tty -s;
		then
			basefile=$(basename "$file" | sed -e 's/[^a-zA-Z0-9._-]/-/g')

			if [ ! -e $file ];
			then
				echo "File $file doesn't exists."
				return 1
			fi

			if [ -d $file ];
			then
				# zip directory and transfer
				zipfile=$( mktemp -t transferXXX.zip )
				cd $(dirname $file) && zip -r -q - $(basename $file) >> $zipfile
				curl --progress-bar --upload-file "$zipfile" "https://transfer.sh/$basefile.zip" >> $tmpfile
				rm -f $zipfile
			else
				# transfer file
				curl --progress-bar --upload-file "$file" "https://transfer.sh/$basefile" >> $tmpfile
			fi
		else
			# transfer pipe
			curl --progress-bar --upload-file "-" "https://transfer.sh/$file" >> $tmpfile
		fi

		# cat output link
		cat $tmpfile

		# cleanup
		rm -f $tmpfile
	}

fi
