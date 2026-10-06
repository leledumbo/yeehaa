export PATH=/opt/LAMW/sdk/platform-tools:$PATH
export GRADLE_HOME=/opt/LAMW/gradle/
export PATH=$PATH:$GRADLE_HOME/bin
source ~/.bashrc
gradle clean build --info
