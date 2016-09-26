FROM java:8

CMD ["java", "-cp", "/root/standalone.jar", "clojure.main", "-m", "todomvc.service.main"]

ADD target/todomvc-standalone.jar /root/standalone.jar
