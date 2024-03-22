set JAVA_HOME=/usr/lib/jvm/java-8-openjdk
path %JAVA_HOME%/bin;%path%
cd /mnt/Data/Works/Free/yeehaa/androiddemo
jarsigner -verify -verbose -certs /mnt/Data/Works/Free/yeehaa/androiddemo/build/outputs/apk/release/androiddemo-release.apk
