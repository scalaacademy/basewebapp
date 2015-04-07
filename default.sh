
RUN_AT="/fast/work/scalaacademy/basewebapp"

NAME="$(basename $(pwd))"

SSHUSER="root"
SSHHOST="XXX.XXX.XXX.XXX"
SSHAUTH="-i /home/david/.ssh/id_rsa"

VPS="DO"

WAR_FILENAME=basewebapp_2.11-0.1.war
WAR="target/scala-2.11/$WAR_FILENAME"
WAR_XML="basewebapp.xml"
TARGET="/opt/$JETTY_VER"

SBT_PROJ_PATH="./"

JETTY_PORT=8080

BACKUPS_DIR="$RUN_AT/.backup"

JAVA_OPTIONS="-XX:MaxPermSize=350M -Xmx500M -server -Drun.mode=production -Dnetworkaddress.cache.ttl=-1"
JETTY_ARGS=""

GIT_TAG=true


