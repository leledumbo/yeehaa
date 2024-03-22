export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
cd /mnt/Data/Works/Free/yeehaa/androiddemo
LC_ALL=C keytool -genkey -v -keystore androiddemo-release.keystore -alias androiddemo.keyalias -keyalg RSA -keysize 2048 -validity 10000 < /mnt/Data/Works/Free/yeehaa/androiddemo/keytool_input.txt
