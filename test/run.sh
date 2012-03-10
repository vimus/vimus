#!/bin/bash

cd "`dirname $0`"
runhaskell -hide-all-packages \
           -packagebase \
           -packagetest-framework \
           -packagetest-framework-quickcheck2 \
           -packagetest-framework-th \
           -packageQuickCheck \
           -packagencursesw \
           -packagemtl \
           -i../src/ \
           -DTEST \
           ListWidgetTest.hs --maximum-generated-tests=5000

runhaskell -DTEST -i../src Spec.hs
