#
# apt simplifier
#

apt() {
  case "$1" in
    -S)    apt-get install ${@:2}                       ;;
    -Ss)   apt-cache search ${@:2}                      ;;
    -Rs)   apt-get remove ${@:2}                        ;;
    -Syu)  apt-get update && apt-get upgrade            ;;
    -Q)    dpkg -l                                      ;;
    # -Qs)
    *)     echo "error: invalid option $1"              ;;
  esac
}
