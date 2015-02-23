BEGIN {FS = ":"; sq = sprintf("%c", 39);ORS=";"}
$1 !~ /#/ { if ($1==printer) print sprintf("set printtype=%s%s%s",sq,$2,sq)
            if ($1==printer) print sprintf("set printcmd=%s%s%s",sq,$3,sq)
            if ($1==printer) print sprintf("set textfilter=%s%s%s",sq,$4,sq)
            if ($1==printer) exit }
