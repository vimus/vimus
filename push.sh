#!/bin/sh

git push origin master && git checkout release && git rebase master && git push origin release -f && git checkout master
