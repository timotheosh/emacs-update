#!/bin/sh
SYSTEM_NAME=emacs-update
COMMAND="(asdf:operate :build-op :${SYSTEM_NAME})"
ROS=$(which ros)
SBCL=$(which sbcl)

rm -rf target/

if [ -n ${ROS} ];then
	${ROS} run -e "(ql:quickload :${SYSTEM_NAME})" -e "(asdf:make :${SYSTEM_NAME})" -q
elif [ -n ${SBCL} ];then
    ${SBCL} --eval ${COMMAND} --quit
else
    echo "Cannot find roswell, nor sbcl. If you have a Common Lisp implementation installed with ASDF, just eval ${COMMAND} to build project."
fi

