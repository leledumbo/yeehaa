export JAVA_HOME=${/usr/libexec/java_home}
export PATH=${JAVA_HOME}/bin:$PATH
cd /mnt/Data/Works/Free/yeehaa/androiddemo
jarsigner -verify -verbose -certs /mnt/Data/Works/Free/yeehaa/androiddemo/build/outputs/apk/release/androiddemo-release.apk
