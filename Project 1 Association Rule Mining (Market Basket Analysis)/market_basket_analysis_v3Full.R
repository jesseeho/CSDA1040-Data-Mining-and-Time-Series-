library(tm)
library(igraph) # get.data.frame
library(data.table) ##fread
library(plyr)
library(dplyr)
library(arules)
library(arulesViz)

rm(list = ls())


# Load data files
order_products <- fread("data/order_products__prior.csv")
products <- fread("data/products.csv")
departments <- fread("data/departments.csv")

# merge to get list of orders and products
#prod_dept = merge(products, departments, by.x="department_id", by.y="department_id")
order_prod_dept = merge(order_products, products, by.x="product_id", by.y="product_id")

# drop unused columns
order_dept = select(order_prod_dept, -c(product_id, aisle_id, add_to_cart_order, reordered, department_id))
order_dept_unique <- unique(order_dept)

## DATA CLEANSING
# Merge similar products together
order_dept_unique$product_name[order_dept_unique$product_name == "Bag of Organic Bananas"] <- "Banana"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic Banana"] <- "Banana"

order_dept_unique$product_name[order_dept_unique$product_name == "Organic Hass Avocado"] <- "Organic Avocado"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic Avocado"] <- "Avocado"
order_dept_unique$product_name[order_dept_unique$product_name == "Hass Avocados"] <- "Avocado"
order_dept_unique$product_name[order_dept_unique$product_name == "Small Hass Avocado"] <- "Avocado"

order_dept_unique$product_name[order_dept_unique$product_name == "Organic Whole Strawberries"] <- "Strawberries"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic Strawberries"] <- "Strawberries"

order_dept_unique$product_name[order_dept_unique$product_name == "Large Lemon"] <- "Lemons"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic Lemon"] <- "Lemons"

order_dept_unique$product_name[order_dept_unique$product_name == "Baby Seedless Cucumber"] <- "Cucumber"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic English Cucumber"] <- "Cucumber"
order_dept_unique$product_name[order_dept_unique$product_name == "Cucumber Kirby"] <- "Cucumber"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic Cucumber"] <- "Cucumber"
order_dept_unique$product_name[order_dept_unique$product_name == "English Seedless Cucumber"] <- "Cucumber"

order_dept_unique$product_name[order_dept_unique$product_name == "Apple Honeycrisp Organic"] <- "Honeycrisp Apple"

order_dept_unique$product_name[order_dept_unique$product_name == "Organic Half & Half"] <- "Half & Half"

order_dept_unique$product_name[order_dept_unique$product_name == "Organic Peeled Whole Baby Carrots"] <- "Baby Carrots"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic Baby Carrots"] <- "Baby Carrots"

order_dept_unique$product_name[order_dept_unique$product_name == "Organic Large Extra Fancy Fuji Apple"] <- "Fuji Apple"
order_dept_unique$product_name[order_dept_unique$product_name == "Organic Fuji Apple"] <- "Fuji Apple"

order_dept_unique$product_name[order_dept_unique$product_name == "Organic Yellow Onion"] <- "Yellow Onion"
order_dept_unique$product_name[order_dept_unique$product_name == "Yellow Onions"] <- "Yellow Onion"

order_dept_unique$product_name[order_dept_unique$product_name == "Organic Unsweetened Almond Milk"] <- "Unsweetened Almond Milk"
order_dept_unique$product_name[order_dept_unique$product_name == "Unsweetened Almondmilk"] <- "Unsweetened Almond Milk"

order_dept_unique$product_name <- removeWords(order_dept_unique$product_name, "Organic ")
##

trans <- as(split(order_dept_unique$product_name, order_dept_unique$order_id), "transactions")

# Displaying Transactions & Associations
trans
head(list(trans),3)
summary(trans)
inspect(trans[1:3])

# Filter transactions by size
filter_trans = trans[size(trans)>=3]
summary(filter_trans)
inspect(filter_trans[1:3])
#help(image)
image(trans[1:10000], 
      xlab="Items (Columns)",
      ylab="Transactions (Rows)"
      )

image(filter_trans[1:10000], 
      xlab="Items (Columns)",
      ylab="Transactions (Rows)"
)

itemFrequencyPlot(trans, topN=20, type="absolute")
itemFrequencyPlot(filter_trans, topN=20, type="absolute")

#hist(size(trans), breaks=0:80, xaxt="n", ylim=c(0,10000),
#     main="Number of Items per Basket", xlab="# Items")
#axis(1, at=seq(0,85, by=5), cex.axis=0.8)
#mtext(paste("Total:", length(trans1), "baskets", sum(size(trans1)), "items"))

items_frequencies <- itemFrequency(trans, type="a")


# MINING ASSOCIATIONS WITH THE APRIORI RULE
# APRIORI: min support = 0.001, confidence = 0.25 
rules <- apriori(trans, parameter=list(support=0.001, confidence=0.25))
summary(rules) ##--- Creates 3779 rules
inspect(rules)
# sort by confidence than inspect
rules <- sort(rules, by="confidence", decreasing = TRUE)
inspectDT(rules)

rules_filter <- apriori(filter_trans, parameter=list(support=0.001, confidence=0.25))
summary(rules_filter) ##--- Creates 3779 rules
inspect(rules_filter)
# sort by confidence than inspect
rules_filter <- sort(rules_filter, by="confidence", decreasing = TRUE)
inspect(rules_filter)


# summary(rules) shows the following:
# parameter specification: min support, min confidence
# total number of rules
# distribution of rule length
# summary of quality measures
# information used for creating rules


# PRUNING REDUNDANT RULES
rules.sorted = sort(rules, by="lift")
#subset.matrix = is.subset(rules.sorted, rules.sorted)
#subset.matrix[lower.tri(subset.matrix, diag=T)]=NA
#redundant = colSums(subset.matrix, na.rm = T) >= 1
#rules.pruned = rules.sorted[!redundant]

rules.pruned <- rules.sorted[!is.redundant(rules.sorted)]

summary(rules.pruned)
inspect(rules.pruned)


# VISUALIZING ASSOCIATION RULES
plot(rules.pruned)
plot(rules.pruned, method="graph")

# Graph PLot
plot(sort(rules, by="confidence"), method = "graph")

# Scatter Plot
plot(sort(rules, by="confidence"), method = "scatterplot")

# Parallel Coordinates Plot
plot(sort(rules, by="confidence"), method = "paracoord")

# Matrix Plot
plot(sort(rules, by="confidence"), method = "matrix")

# Item Frequency Plot
itemFrequencyPlot(trans)

# Rules Data Table
rulesdt <- as(rules, "data.frame")
rulesdt


## MINE FREQUENT ITEMSETS WITH ECLAT
# depth first search - vertical data layout
frequentsets = eclat(trans, parameter=list(support=0.01, maxlen=10))
summary(frequentsets)
inspect(sort(frequentsets, by="support"))




# 
# 
# inspect(rules[1:10])
# 
# # Remove redundant rule
# rules <- rules[!is.redundant(rules)]
# rules_dt <- data.table(lhs = labels(lhs(rules)),
#                        rhs = labels(rhs(rules)),
#                        quality(rules))[order(-lift),]
# summary(rules_dt)
# head(rules_dt, 5)
# 
# 
# # APRIORI: min support = 0.001, confidence = 0.8, maxlen=10 --- Creates 0 rules
# rules1 <- apriori(trans1, parameter=list(support=0.001, confidence=0.8, maxlen=10))
# summary(rules1)
# 
# 
# # APRIORI: min support = 0.001, confidence = 0.8, maxlen=3 --- Creates 0 rules
# rules2 <- apriori(trans1, parameter=list(support=0.001, confidence=0.8, maxlen=3))
# summary(rules2)
# 
# # APRIORI: min support = 0.01, confidence = 0.6, maxlen=3 --- Creates 0 rules
# rules3 <- apriori(trans1, parameter=list(support=0.01, confidence=0.6, maxlen=3), control=list(verbose=FALSE))
# summary(rules3)
# 
# # Interpretation & Analysis
# # identify which products were sold how frequently
# 
# 
# # Item Frequency Plot
# if (!require("RColorBrewer")) {
#   # install color packages of RE
#   install.packages("RColorBrewer")
#   # include library RColorBrewer
#   library(RColorBrewer)
# }
# 
# # absolute plots numeric frequencies of each item independently
# itemFrequencyPlot (trans1, topN=20, type="absolute",
#                    col=brewer.pal(8, 'Pastel2'),
#                    main="Absolute Item Frequency Plot")
# 
# # relative plots how many times items appear compared to others
# itemFrequencyPlot(trans1, topN=20, type="relative",
#                   col=brewer.pal(8, 'Pastel2'),
#                   main="Relative Item Frequency Plot")
# 
# # scatterplot
# plotly_arules(rules)
# 
# 
# # Network graph
# subrules2 <- head(sort(rules, by="confidence"), 20)
# ig <- plot(subrules2, method = "graph", control=list(type="items"))
# ig_df <- get.data.frame(ig, what="both")
