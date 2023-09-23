cd $(dirname $0)

cd ../..

git pull

# use default branch
#git checkout master

cd fitlang-pom
mvn install -Dmaven.test.skip=true
cd ..

# pkill node
pkill tail

lsof -i:21111 | awk '{print $2}' | tail -n 1 | xargs -I {} kill -9 {}
sleep 1

cd fitlang-springboot
mvn spring-boot:run > server.log &

tail -f server.log &
