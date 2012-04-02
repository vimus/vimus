#!/bin/sh

git push origin master && git co release && git rebase master && git push origin release -f && git co master
