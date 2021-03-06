#!/bin/sh

# https://www.linode.com/speedtest
: "${LINODE_TEST_TYPE:=100MB}"
: "${LINODE_REGIONS:=newark atlanta dallas fremont frankfurt london singapore tokyo2}"
[ "${LINODE_REGIONS}" = - ] && LINODE_REGIONS=

# http://speedtest-${region}.digitalocean.com/
: "${DO_TEST_TYPE:=100mb}"
: "${DO_REGIONS:=nyc1 nyc2 nyc3 ams2 ams3 sfo1 sfo2 sgp1 lon1 fra1 tor1 blr1}"
[ "${DO_REGIONS}" = - ] && DO_REGIONS=

# https://www.vultr.com/faq/#downloadspeedtests
: "${VULTR_TEST_TYPE:=1000MB}"
: "${VULTR_REGIONS:=fra-de par-fr ams-nl lon-gb nj-us sgp tor-ca sel-kor il-us ga-us fl-us hnd-jp tx-us wa-us sjo-ca-us lax-ca-us syd-au}"
[ "${VULTR_REGIONS}" = - ] && VULTR_REGIONS=

unset http_proxy
unset https_proxy
unset all_proxy

download() {
	curl --silent --output /dev/null --max-time 30 --write-out "%{speed_download}\t\t\t%{size_download}\t\t\t${1}\n" "${2}"
}

{
	for region in $LINODE_REGIONS; do
		download "LINODE:${region}" "http://speedtest.${region}.linode.com/${LINODE_TEST_TYPE}-${region}.bin"
	done
	for region in $DO_REGIONS; do
		download "DO:${region}" "http://speedtest-${region}.digitalocean.com/${DO_TEST_TYPE}.test"
	done
	for region in $VULTR_REGIONS; do
		download "VULTR:${region}" "http://${region}-ping.vultr.com/vultr.com.${VULTR_TEST_TYPE}.bin"
	done
} | sort --numeric-sort -r
