#!/bin/bash
export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
java -jar client/target/scala-2.13/client.jar -p 56787 -s localhost:45678
