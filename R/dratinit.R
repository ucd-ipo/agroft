library(drat)

pkg <- '../agroft_0.2.tar.gz'
insertPackage(pkg, '~/gh-pages', commit='push', action='prune')
