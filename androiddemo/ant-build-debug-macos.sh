export PATH=/opt/LAMW/ant/bin/:$PATH
export JAVA_HOME=${/usr/libexec/java_home}
export PATH=${JAVA_HOME}/bin:$PATH
cd /mnt/Data/Works/Free/yeehaa/androiddemo
ant -Dtouchtest.enabled=true debug
