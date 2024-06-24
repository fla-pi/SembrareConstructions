library('partykit')
library('caret')

#0. Dataset
dataset_sembrare <- read.table('sembrare_dataset.csv', header=T, sep=“;”)

##0.1 Categorical variables to factor
dataset_sembrare$Corpus <- as.factor(dataset_sembrare$ï..Corpus)
dataset_sembrare$Constructions <- as.factor(dataset_sembrare$Constructions)
dataset_sembrare$TypeOfAnaphor <- as.factor(dataset_sembrare$TypeOfAnaphor)
dataset_sembrare$Animacy <- as.factor(dataset_sembrare$Animacy)
dataset_sembrare$Accessibility <- as.factor(dataset_sembrare$Accessibility)

# 1. Tree with all predictors
tree <- ctree(Constructions ~ Corpus + TextGenre_Ordinal + Subj_Length + TypeOfAnaphor + Animacy + Accessibility, data = dataset_sembrare, control = ctree_control(minsplit = 5L, minbucket = 5L, testtype = “MonteCarlo”))
tree

'''
Model formula:
  Constructions ~ Corpus + TextGenre_Ordinal + Subj_Length + TypeOfAnaphor + 
  Animacy + Accessibility

Fitted party:
  [1] root
|   [2] TextGenre_Ordinal <= 4
|   |   [3] TypeOfAnaphor in Mediated_event, Mediated_general, Mediated_poss, Mediated_set, Mediated_sit, New, Old_event, Old_general, Old_generic, Old_ident: mi sembra che (n = 148, err = 37.8%)
|   |   [4] TypeOfAnaphor in Old_rel: sembrare+infinitive (n = 6, err = 33.3%)
|   [5] TextGenre_Ordinal > 4
|   |   [6] TextGenre_Ordinal <= 6
|   |   |   [7] TypeOfAnaphor in Mediated_general, Mediated_poss, Mediated_sit, Old_generic, Old_ident, Old_rel: sembrare+infinitive (n = 91, err = 51.6%)
|   |   |   [8] TypeOfAnaphor in New, Old_event, Old_general: sembra che (n = 20, err = 55.0%)
|   |   [9] TextGenre_Ordinal > 6: sembrare+infinitive (n = 133, err = 29.3%)

Number of inner nodes:    4
Number of terminal nodes: 5
'''

## 1.1. Accuracy and other metrics
pred_corp <- predict(tree)
confusionMatrix(data = pred_corp, reference = dataset_sembrare$Constructions)

'''
Confusion Matrix and Statistics

Reference
Prediction            mi sembra che sembra che sembrare+infinitive
mi sembra che                  92         48                   8
sembra che                      8          9                   3
sembrare+infinitive            23         65                 142

Overall Statistics

Accuracy : 0.6106          
95% CI : (0.5607, 0.6587)
No Information Rate : 0.3844          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.3986          

Mcnemars Test P-Value : < 2.2e-16       

Statistics by Class:

             Class: mi sembra che Class: sembra che Class: sembrare+infinitive
Sensitivity                0.7480           0.07377                     0.9281
Specificity                0.7964           0.96014                     0.6408
Pos Pred Value             0.6216           0.45000                     0.6174
Neg Pred Value             0.8760           0.70106                     0.9345
Prevalence                 0.3090           0.30653                     0.3844
Detection Rate             0.2312           0.02261                     0.3568
Detection Prevalence       0.3719           0.05025                     0.5779
Balanced Accuracy          0.7722           0.51696                     0.7845
'''

## 1.2. Conditional Variable Importance
varimp_textcorp <- varimp(tree, conditional = T)
varimp_textcorp

'''
TextGenre_Ordinal     TypeOfAnaphor 
        0.5046966         0.0999621 
'''


#2. Tree without text genres
tree_corpus <- ctree(Constructions ~ Corpus + Subj_Length + TypeOfAnaphor + Animacy + Accessibility, data = dataset_sembrare, control = ctree_control(minsplit = 5L, minbucket = 5L, testtype = “MonteCarlo”))
tree_corpus

'''
Model formula:
Constructions ~ Corpus + Subj_Length + TypeOfAnaphor + Animacy + Accessibility

Fitted party:
[1] root
|   [2] Corpus in SpokenCorpora
|   |   [3] TypeOfAnaphor in Mediated_event, Mediated_general, Mediated_poss, Mediated_set, Mediated_sit, New, Old_event, Old_general, Old_generic, Old_ident
|   |   |   [4] TypeOfAnaphor in Mediated_event, Old_generic: sembra che (n = 10, err = 10.0%)
|   |   |   [5] TypeOfAnaphor in Mediated_general, Mediated_poss, Mediated_set, Mediated_sit, New, Old_event, Old_general, Old_ident: mi sembra che (n = 167, err = 41.9%)
|   |   [6] TypeOfAnaphor in Old_rel: sembrare+infinitive (n = 12, err = 25.0%)
|   [7] Corpus in WrittenCorpus: sembrare+infinitive (n = 209, err = 39.2%)

Number of inner nodes:    3
Number of terminal nodes: 4
'''

## 2.1. Accuracy and other metrics
pred_corp2 <- predict(tree_corpus)
confusionMatrix(data = pred_corp2, reference = dataset_sembrare$Constructions)

'''
Confusion Matrix and Statistics

                     Reference
Prediction            mi sembra che sembra che sembrare+infinitive
  mi sembra che                  97         54                  16
  sembra che                      0          9                   1
  sembrare+infinitive            26         59                 136

Overall Statistics
                                          
               Accuracy : 0.608           
                 95% CI : (0.5582, 0.6563)
    No Information Rate : 0.3844          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.3962          
                                          
 Mcnemars Test P-Value : < 2.2e-16       

Statistics by Class:
  
  Class: mi sembra che Class: sembra che Class: sembrare+infinitive
Sensitivity                0.7886           0.07377                     0.8889
Specificity                0.7455           0.99638                     0.6531
Pos Pred Value             0.5808           0.90000                     0.6154
Neg Pred Value             0.8874           0.70876                     0.9040
Prevalence                 0.3090           0.30653                     0.3844
Detection Rate             0.2437           0.02261                     0.3417
Detection Prevalence       0.4196           0.02513                     0.5553
Balanced Accuracy          0.7670           0.53507                     0.7710
'''

## 2.2. Conditional Variable Importance
varimp_corp <- varimp(tree_corpus, conditional = T)
varimp_corp

'''
Corpus TypeOfAnaphor 
0.4450566     0.4399372
'''

# 3. No extra-linguistic factors
tree_nocontext <- ctree(Constructions ~ Subj_Length + TypeOfAnaphor + Animacy + Accessibility, data = dataset_sembrare, control = ctree_control(minsplit = 5L, minbucket = 5L, testtype = “MonteCarlo”))
tree_nocontext 

'''
Model formula:
  Constructions ~ Subj_Length + TypeOfAnaphor + Animacy + Accessibility

Fitted party:
  [1] root
|   [2] TypeOfAnaphor in Mediated_event, Mediated_general, Mediated_set, Mediated_sit, New, Old_event, Old_general, Old_generic, Old_ident
|   |   [3] TypeOfAnaphor in Mediated_event, Mediated_set, Old_general, Old_generic: sembra che (n = 42, err = 45.2%)
|   |   [4] TypeOfAnaphor in Mediated_general, Mediated_sit, New, Old_event, Old_ident
|   |   |   [5] Subj_Length <= 0: mi sembra che (n = 96, err = 59.4%)
|   |   |   [6] Subj_Length > 0: sembrare+infinitive (n = 204, err = 54.9%)
|   [7] TypeOfAnaphor in Mediated_part, Mediated_poss, Old_rel: sembrare+infinitive (n = 56, err = 26.8%)

Number of inner nodes:    3
Number of terminal nodes: 4
'''

## 3.1. Accuracy and other metrics
pred_corp2 <- predict(tree_nocontext)
confusionMatrix(data = pred_corp2, reference = dataset_sembrare$Constructions)

'''
Confusion Matrix and Statistics

Reference
Prediction            mi sembra che sembra che sembrare+infinitive
mi sembra che                  39         39                  18
sembra che                     17         23                   2
sembrare+infinitive            67         60                 133

Overall Statistics

Accuracy : 0.4899          
95% CI : (0.4398, 0.5402)
No Information Rate : 0.3844          
P-Value [Acc > NIR] : 1.197e-05       

Kappa : 0.2055          

Mcnemars Test P-Value : < 2.2e-16       

Statistics by Class:

             Class: mi sembra che Class: sembra che Class: sembrare+infinitive
Sensitivity               0.31707           0.18852                     0.8693
Specificity               0.79273           0.93116                     0.4816
Pos Pred Value            0.40625           0.54762                     0.5115
Neg Pred Value            0.72185           0.72191                     0.8551
Prevalence                0.30905           0.30653                     0.3844
Detection Rate            0.09799           0.05779                     0.3342
Detection Prevalence      0.24121           0.10553                     0.6533
Balanced Accuracy         0.55490           0.55984                     0.6755
'''

## 3.2. Conditional Variable Importance
varimp_nocontext <- varimp(tree_nocontext, conditional = T)
varimp_nocontext

'''
TypeOfAnaphor   Subj_Length 
    0.1530736     0.0715991
'''
