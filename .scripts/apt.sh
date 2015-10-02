#
# apt simplifier
#

pacapt() {
  case "$1" in
    -S)    sudo apt-get install ${@:2} ;;
    -Ss)   apt-cache search ${@:2} ;;
    -Sy)   sudo apt-get update ;;
    -Su)   sudo apt-get upgrade ;;
    -Syu)  sudo apt-get update && sudo apt-get upgrade ;;

    -Rs)   sudo apt-get remove ${@:2} ;;

    -Q)    dpkg -l | awk '{print $2" "$3'} ;;
    -Qs)   dpkg -l | \
            grep $(printf '%q' "${@:2}") | \
            awk '{ print $2" "$3; printf "   "; for(i=5;i<=NF;i++) printf " "$i; print "" }' ;;

    -U)    for file in ${@:2}; do
             sudo dpkg -i $file
             sudo apt-get install -f
           done

    *)     echo "error: invalid option $1" ;;
  esac
}
