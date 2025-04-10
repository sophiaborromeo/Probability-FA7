
# Formative Assessmber #7

Authors:

Borromeo, Elisha

Mayo, Zyann

Mercado, Consuelo

Sinocruz, Arvie

Tagaytay, Gabriel

Date: 2025-04-10

---


# **Campus-Related Issues Using Exponential Distribution**

## 1. Identify a Practical Campus Problem

For this assessment, the group identified the time between students arriving to queue for an elevator in the Education Building inside the FEU Manila campus—a practical campus problem where events occur randomly over time.

## 2. Collect Data

The data collection started at 1:40 PM and ended at 2:10 PM.

The table below shows the time intervals between the arrival of each
person. The time is standardized.

```{r, echo=FALSE}
library(knitr)

person <- 1:30
time_secs <- c(
  62, 45, 22.78, 2, 104.2, 43, 104.2, 115.34, 91.63, 29.25,
  36.01, 7.36, 13.82, 61.91, 11.75, 2.81, 15.46, 75.10, 85.47,
  10.99, 13.6, 131.94, 41.26, 28.84, 46.69, 21.80, 53.86, 202.49,
  120, 40.91
)

arrival_times <- data.frame(
  Person = person,
  Arrival_Time_Seconds = round(time_secs, 2)  
)

kable(arrival_times, 
      caption = "Time Between Student Arrivals at the Elevator Queue (in Seconds)",
      align = "c")  
```

## 3. Verify if Exponential Distribution is Applicable

### Instruction 1: Check if events occur randomly and independently over time.

Because the group recorded interarrival times, this implies independence—since each record is a time interval not influenced by the last one. Since students come from random locations with different purposes in queueing, this also indicates random occurrence. It's safe to assume the events are independent and random over time. Therefore, we can apply the exponential distribution.

### Instruction 2: Identify the average rate of event occurrence per unit of time.

The formula for lambda is:

$$
\text{Lambda} = \frac{n}{\sum (\text{arrival times})}
$$

Where $n$ is the number of events, and $\sum (\text{waiting times})$ is
the sum of all the arrival times. Through manual calculation, we get:

$$
\text{Lambda} = \frac{30}{1,641.47}
$$ $$
\text{Lambda} = {0.0182763}
$$

For number 4, we will be calculating lambda using a statistical tool.

## 4. Compute Key Parameters

### Mean (Expected Value)

For the exponential distribution, the mean (expected value $\mu$) is the
reciprocal of the rate $\lambda$:

$$
\lambda = \frac{\text{Number of Observations}}{\text{Total Arrival Time}}
$$ $$
\mu = \frac{1}{\lambda}
$$

```{r, echo=FALSE}
time_secs <- c(
  62, 45, 22.78, 2, 104.2, 43, 104.2, 115.34, 91.63, 29.25,
  36.01, 7.36, 13.82, 61.91, 11.75, 2.81, 15.46, 75.10, 85.47,
  10.99, 13.6, 131.94, 41.26, 28.84, 46.69, 21.80, 53.86, 202.49,
  120, 40.91
)

n <- length(time_secs)
total_time <- sum(time_secs)
lambda <- n / total_time
mean_time <- 1 / lambda

```
Mean arrival time: **`r paste0(round(mean_time, 3))` seconds**.

### Probability Density Function (PDF)

$$
f(x) = \lambda e^{-\lambda x}, \quad \text{for } x \geq 0
$$

```{r, echo=FALSE}
time_secs <- c(
  62, 45, 22.78, 2, 104.2, 43, 104.2, 115.34, 91.63, 29.25,
  36.01, 7.36, 13.82, 61.91, 11.75, 2.81, 15.46, 75.10, 85.47,
  10.99, 13.6, 131.94, 41.26, 28.84, 46.69, 21.80, 53.86, 202.49,
  120, 40.91
)

n <- length(time_secs)
total_time <- sum(time_secs)
lambda <- n / total_time

pdf_vals <- lambda * exp(-lambda * time_secs)


pdf_results <- data.frame(
  Time_Seconds = round(time_secs, 2),
  PDF = round(pdf_vals, 5)
)

library(knitr)
kable(pdf_results, 
      caption = "PDF Values for Each Data Point",
      align = "c")

```

### Cumulative Distribution Function (CDF)

$$
F(x) = 1 - e^{-\lambda x}, \quad \text{for } x \geq 0
$$

```{r, echo=FALSE}
time_secs <- c(
  62, 45, 22.78, 2, 104.2, 43, 104.2, 115.34, 91.63, 29.25,
  36.01, 7.36, 13.82, 61.91, 11.75, 2.81, 15.46, 75.10, 85.47,
  10.99, 13.6, 131.94, 41.26, 28.84, 46.69, 21.80, 53.86, 202.49,
  120, 40.91
)

n <- length(time_secs)
total_time <- sum(time_secs)
lambda <- n / total_time

cdf_vals <- 1 - exp(-lambda * time_secs)

cdf_results <- data.frame(
  Time_Seconds = round(time_secs, 2),
  CDF = round(cdf_vals, 5)
)

library(knitr)
kable(cdf_results, caption = "CDF Values for Each Data Point", align = "c")
```

# 5. Interpret the Results

### Determine the likelihood of an event happening within a specific timeframe.

We can use the exponential Cumulative Distribution Function (CDF) to
find the probabilities, 

**a. Probability of a student arriving within 30 seconds:** 
$$
F(x) = 1 - e^{-\lambda x}
$$

```{r}
lambda <- 30 / sum(time_secs)
time_30 <- 30
prob_within_30 <- 1 - exp(-lambda * time_30)
```
Probability a student arrives within 30 seconds: **`r paste0(round(prob_within_30 * 100, 2), "%")` **

**b. Probability of student arriving more than 60 seconds:** 
$$
P(X > x) = e^{-\lambda x}
$$

```{r}
time_60 <- 60
prob_more_than_60 <- exp(-lambda * time_60)
```
Probability of student arriving more than 60 seconds: **`r paste0(round(prob_more_than_60 * 100, 2), "%")` **

### Implications

**a. Expected Arrival Time:** Based on the data, the mean waiting time
between student arrivals is approximately: 

```{r}
mean_wait <- 1 / lambda
```

On average, a student arrives every **`r paste0(round(mean_wait, 2))` seconds **.

**b. Real-World Implications on Campus**

*Factor Affecting Queuing*

-   It was observed that the intervals between arrivals of the FEU community, especially students arriving at the elevator in Education Building were shorter at around 1:30 PM to 1:45 PM since it was the usual starting time of classes (Peak Periods). This means that the queue was longer. As time passed by, specifically after 1:45 PM (Off-Peak Periods), the intervals between student arrivals started to increase. This is because, as more professors and students proceeded to their respective classes, fewer students or professors required the use of the elevator, causing a larger interval between arrivals. As a result, the queue got thinner, and waiting times between students grew, as can be inferred from the data obtained.

*Elevator Usage Efficiency:*

-   There is only a 42% chance that a student or professor arrives within 30 seconds and as the group observed there is a chance that elevators often leave with only one or two students resulting in a wastage of energy and time.

*Queue Management:*

-   With a mean student arrival time of *54.7 seconds*, a line can form during peak hours. This implies that during rush hours, such as at the beginning of classes, the intervals between student arrivals can be shorter, resulting in a more congested queue and possibly faster boarding times. Conversely, at off-peak times, when students are arriving in smaller numbers, waiting times between arrivals can become longer. The average waiting time of 54.7 seconds serves as a valuable benchmark for estimating how long a group of students will have to wait to enter the elevator. By knowing these trends, the school can better predict peak periods and schedule the elevator system appropriately, resulting in smoother operations and reduced wait times during peak periods.

*Facility Optimization:*

-   Elevator scheduling or idle-time automation could be improved using
    real-time arrival predictions.

-   Digital displays showing estimated wait times could enhance the
    student experience.

*Energy Efficiency:*

-   Since there is a 33% chance of waiting more than 60 seconds, the
    elevator could delay activation unless more people arrive,
    optimizing both energy and time.

## Visualization:

```{r}
library(ggplot2)

# Define lambda
lambda <- 30 / sum(time_secs)

# Define a sequence of time values (x)
x_vals <- seq(0, 250, by = 1)

# Compute PDF and CDF for each x
pdf_vals <- lambda * exp(-lambda * x_vals)
cdf_vals <- 1 - exp(-lambda * x_vals)

# Combine into a data frame
exp_data <- data.frame(
  Time = x_vals,
  PDF = pdf_vals,
  CDF = cdf_vals
)

# Plot
ggplot(exp_data, aes(x = Time)) +
  geom_line(aes(y = PDF, color = "PDF"), linewidth = 1.2) +
  geom_line(aes(y = CDF, color = "CDF"), linewidth = 1.2, linetype = "dashed") +
  labs(title = "Exponential Distribution: PDF vs. CDF",
       x = "Time (seconds)",
       y = "Value",
       color = "Function") +
  theme_minimal() +
  scale_color_manual(values = c("PDF" = "steelblue", "CDF" = "darkorange"))

```

### **Interpretation** 

The graph shows the relationship between the Cumulative Distribution Function (CDF) and the Probability Density Function (PDF) of an exponential distribution. The CDF, in the orange dashed line, rises slowly and approaches 1 asymptotically, showing that as time goes by, the likelihood of the event increases. Conversely, the PDF, shown as the blue solid line, starts with a larger value and then decays exponentially, consistent with the fact that the chance of occurrence of the event decreases as time increases. This is an example or characteristic of exponential distributions, in which events have higher chances of occurrence earlier than later.
    
