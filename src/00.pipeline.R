# development <- corpora$development
# reshaped <- corpora$reshaped
# clean <-corpora$clean
# 
# # Reshape Data
# reshapeData(development, reshaped)
# 
# # Clean Data
# parallelizeTask(cleanData, reshaped, clean, referenceFiles, regexPatterns)
# 
# # Analyze Data
# cleanDataAnalysis <- analyzeData('fast', clean, referenceFiles, directories, regexPatterns) 
# 
# # Create VGC
# vgc <- parallelizeTask(createVGC, clean, directories)
# 
# # Create Zipf Objects
# zipf <- parallelizeTask(createZipfObjects, clean, vgc)
# 
# # Create Sample Size Estimate
# ss <- createSampleSize(zipf, directories)