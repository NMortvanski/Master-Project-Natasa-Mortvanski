#!/bin/bash

##qiime version 2020.8.0

#from https://john-quensen.com/tutorials/merging-dada2-results-in-qiime2/

#Activate QIIME2 environment

#conda activate qiime2

# Merge two ASV tables into one -> Hospital's CDI samples and donors samples

qiime feature-table merge \
 --i-tables table-CDI.qza \
 --i-tables table-donor.qza \
 --o-merged-table merged-table.qza
 
# Merge representative sequences
 
qiime feature-table merge-seqs \
 --i-data rep-seqs-CDI.qza \
 --i-data rep-seqs-donor.qza \
 --o-merged-data rep-seqs-merged.qza
 
#Generate the phylogenetic tree for the ASVs

qiime phylogeny align-to-tree-mafft-fasttree \
 --i-sequences rep-seqs-merged.qza \
 --o-alignment aligned-rep-seqs.qza \
 --o-masked-alignment masked-aligned-rep-seqs.qza \
 --o-tree unrooted-tree.qza \
 --o-rooted-tree rooted-tree-merged.qza
 
# Generate taxonomy file

wget \
 -O "gg-13-8-99-nb-classifier.qza" \
  "https://data.qiime2.org/2020.8/common/gg-13-8-99-nb-classifier.qza"

qiime feature-classifier classify-sklearn \
 --i-reads rep-seqs-merged.qza \
 --i-classifier gg-13-8-99-nb-classifier.qza \
 --o-classification taxonomy-merged.qza

