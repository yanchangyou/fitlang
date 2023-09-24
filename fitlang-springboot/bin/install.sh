
yum install git java unzip lsof -y

yum install https://repos.fedorapeople.org/repos/dchen/apache-maven/epel-7/x86_64/apache-maven-3.5.2-1.el7.noarch.rpm -y

git clone https://gitee.com/imflyfish/fitlang

cd fitlang/fitlang-springboot

wget https://gitee.com/imflyfish/fitlang/raw/serverless/fitlang-springboot/doc/settings.xml -O ~/.m2/settings.xml

bin/start.sh
