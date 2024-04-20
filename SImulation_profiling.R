
library(tidyverse)

# load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_50_100_mtmm.Rdata")
# load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_50_100_f_1f_r.Rdata")
# load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_250_mtmm.Rdata")
# load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_250_f_1f_r.Rdata")


load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_processed_results_250_mtmm.Rdata")
results_table1 <- results_table
load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_processed_results_250_f_1f_r.Rdata")
results_table2 <- results_table


results <- rbind(results_table1, results_table2)

results %>% filter(model_type == "fixed") -> results_fixed

results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_fixed %>% summarise(good = 100*mean(good_estimate))




results %>% filter(model_type == "one-factor") %>% filter(precision != 3)-> results_1f

results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias_pc))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_1f %>% group_by(integral_type, data_type) %>% summarise(good = 100*mean(good_estimate))


results_1f %>% group_by(integral_type, data_type, precision) %>% summarise(good = mean(coverage_probability))









results_table %>% filter(model_type == "random")  -> results_random

results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_random %>% group_by(integral_type, data_type) %>% summarise(good = 100*mean(good_estimate))


results_random %>% group_by(integral_type, data_type, precision) %>% summarise(good = mean(coverage_probability))




results %>% filter(model_type == "mtmm")  -> results_mtmm

results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias_pc))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()

results_mtmm %>% group_by(integral_type, data_type) %>% summarise(good = 100*mean(good_estimate))


results_mtmm %>% group_by(integral_type, data_type, precision) %>% summarise(good = mean(coverage_probability))




