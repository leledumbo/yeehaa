export JAVA_HOME=${/usr/libexec/java_home}
export PATH=${JAVA_HOME}/bin:$PATH
cd /mnt/Data/Works/Free/yeehaa/androiddemo
keytool -genkey -v -keystore androiddemo-release.keystore -alias androiddemo.keyalias -keyalg RSA -keysize 2048 -validity 10000 < /mnt/Data/Works/Free/yeehaa/androiddemo/keytool_input.txt
