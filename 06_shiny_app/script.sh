#!/bin/bash

source activate qiime2

given_file=$1 # retrieve input parameter and populate a variable


if test -d $given_file  ; then
     qiime tools import \
         --type 'SampleData[PairedEndSequencesWithQuality]' \
  	 --input-path $given_file \
  	 --input-format CasavaOneEightSingleLanePerSampleDirFmt \
  	 --output-path demux-paired-end.qza

     qiime dada2 denoise-paired \
	  --i-demultiplexed-seqs demux-paired-end.qza \
	  --p-trim-left-f 15 \
	  --p-trim-left-r 0 \
	  --p-trunc-len-f 280 \
	  --p-trunc-len-r 220 \
	  --o-table table-dada2.qza \
	  --o-representative-sequences rep-seqs-dada2.qza \
	  --o-denoising-stats denoising-stats-dada2.qza \
	  --verbose

     qiime phylogeny align-to-tree-mafft-fasttree \
	  --i-sequences rep-seqs-dada2.qza \
	  --o-alignment aligned-rep-seqs.qza \
	  --o-masked-alignment masked-aligned-rep-seqs.qza \
	  --o-tree unrooted-tree.qza \
	  --o-rooted-tree rooted-tree.qza

     qiime feature-classifier classify-sklearn \
 	 --i-reads rep-seqs-dada2.qza \
 	 --i-classifier gg-13-8-99-nb-classifier.qza \
 	 --o-classification taxonomy.qza
	  
     qiime diversity alpha \
	  --i-table table-dada2.qza  \
	  --p-metric gini_index \
	  --o-alpha-diversity gini_index_vector.qza
	
     qiime diversity alpha-phylogenetic \
	  --i-table table-dada2.qza \
	  --i-phylogeny rooted-tree.qza \
	  --p-metric faith_pd \
	  --o-alpha-diversity faith_pd_vector.qza
	  
     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric chao1 \
	  --o-alpha-diversity chao1_vector.qza

     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric fisher_alpha \
	  --o-alpha-diversity fisher_alpha_vector.qza
	  
     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric margalef \
	  --o-alpha-diversity margalef_vector.qza
	  
     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric menhinick \
	  --o-alpha-diversity menhinick_vector.qza
	  
     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric simpson \
	  --o-alpha-diversity simpson_vector.qza
	  
     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric strong \
	  --o-alpha-diversity strong_vector.qza
	
     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric pielou_e \
	  --o-alpha-diversity pielou_vector.qza
	  
     qiime diversity alpha \
	  --i-table table-dada2.qza \
	  --p-metric shannon \
	  --o-alpha-diversity shannon_vector.qza

	  
     qiime tools export \
	  --input-path gini_index_vector.qza \
	  --output-path .
	  
     mv alpha-diversity.tsv gini.tsv
	  
     qiime tools export \
	  --input-path faith_pd_vector.qza \
	  --output-path .

     mv alpha-diversity.tsv faith.tsv

	  
     qiime tools export \
	  --input-path chao1_vector.qza \
	  --output-path .

     mv alpha-diversity.tsv chao1.tsv
     
     qiime tools export \
	  --input-path fisher_alpha_vector.qza \
	  --output-path .
	  
     mv alpha-diversity.tsv fisher_alpha.tsv
	  
     qiime tools export \
	  --input-path margalef_vector.qza \
	  --output-path .

     mv alpha-diversity.tsv margalef.tsv

     qiime tools export \
	  --input-path menhinick_vector.qza \
	  --output-path .
	  
     mv alpha-diversity.tsv menhinick.tsv
	  
     qiime tools export \
	  --input-path simpson_vector.qza \
	  --output-path .

     mv alpha-diversity.tsv simpson.tsv

     qiime tools export \
	  --input-path strong_vector.qza \
	  --output-path .
	  
     mv alpha-diversity.tsv strong.tsv
	  
     qiime tools export \
	  --input-path pielou_vector.qza \
	  --output-path .

     mv alpha-diversity.tsv pielou.tsv
     
     qiime tools export \
	  --input-path shannon_vector.qza \
	  --output-path .

     mv alpha-diversity.tsv shannon.tsv
     

else
    echo no file 
fi

echo Finished!

	
