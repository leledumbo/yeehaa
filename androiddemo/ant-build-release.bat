set Path=%PATH%;/opt/LAMW/ant/bin/
set JAVA_HOME=/usr/lib/jvm/java-8-openjdk
cd /mnt/Data/Works/Free/yeehaa/androiddemo
call ant clean release
if errorlevel 1 pause
