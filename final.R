gr <- read.csv("C:/Users/BonBon/OneDrive/Máy tính/xác suất thống kê/All_GPUs.csv", header = TRUE)
head(gr)

#chon cac cot quan trong
gpu_col <- gr[, c("Manufacturer", "Best_Resolution", "Core_Speed" , "Dedicated" , "Integrated" , "Memory" , "Memory_Bandwidth" , "Memory_Speed" , "Name" , "Pixel_Rate" , "Release_Date" , "Release_Price" , "Resolution_WxH" , "TMUs" , "Texture_Rate")]
head(gpu_col)


#xu li du lieu khuyet
gpu_col$Memory <- as.numeric(sub(" MB", "", gpu_col$Memory))
gpu_col$Memory_Bandwidth <- as.numeric(sub("\\s*GB/sec", "", gpu_col$Memory_Bandwidth))
gpu_col$Memory_Speed <- as.numeric(sub(" MHz", "", gpu_col$Memory_Speed))
gpu_col$Pixel_Rate <- as.numeric(sub(" GPixel/s","", gpu_col$Pixel_Rate))
gpu_col$Texture_Rate <- as.numeric(sub(" GTexel/s","", gpu_col$Texture_Rate))
head(gpu_col)

#lam sach du lieu khuyet
#kiem tra co bao nhieu du lieu bi khuyet
apply(is.na(gpu_col), 2, sum)
#kiem tra vi tri chinh xac cua du lieu bi khuyet
apply(is.na(gpu_col), 2, which)
#tinh ti le du lieu khuyet
apply(is.na(gpu_col), 2 , mean)


#xoa du lieu bi khuyet cua cot memoryspeed, memorybandwidth
gpu_col <- subset(gpu_col, !is.na(Memory_Speed) & !is.na(Memory_Bandwidth))
#tinh gia median cua cac cot memory, pixelrate, texturerate, TMUs
median_memory <- median(gpu_col$Memory, na.rm = TRUE)
median_pixel <- median(gpu_col$Pixel_Rate, na.rm = TRUE)
median_texel <- median(gpu_col$Texture_Rate, na.rm = TRUE)
median_tmu <- median(gpu_col$TMUs, na.rm = TRUE)
#thay the gia tri NA trong cac cot tren bang median
gpu_col$Memory <- ifelse(is.na(gpu_col$Memory), median_memory, gpu_col$Memory)
gpu_col$Pixel_Rate <- ifelse(is.na(gpu_col$Pixel_Rate), median_pixel, gpu_col$Pixel_Rate)
gpu_col$Texture_Rate <- ifelse(is.na(gpu_col$Texture_Rate), median_texel, gpu_col$Texture_Rate)
gpu_col$TMUs <- ifelse(is.na(gpu_col$TMUs), median_tmu, gpu_col$TMUs)
#kiem tra lai du lieu
apply(is.na(gpu_col), 2 , sum)



#thongke mo ta
#toc do bo nho memory
library(dplyr)
library(knitr)
nF_sum <- gpu_col %>%
	group_by(Manufacturer) %>%
	summarize(
		sample_size = n(), 
		mean = mean(Memory_Speed, na.rm = TRUE), 
		sd = sd(Memory_Speed, na.rm = TRUE), 
		minimum = min(Memory_Speed, na.rm = TRUE), 
		first_quantile = quantile(Memory_Speed, 0.25, na.rm = TRUE), 
		median = median(Memory_Speed, na.rm = TRUE), 
		third_quantile = quantile(Memory_Speed, 0.75, na.rm = TRUE), 
		maximum = max(Memory_Speed, na.rm = TRUE)
		)
head(nF_sum)
#ve boxplot Memory_Speed tuong ung voi tung hang 
library(ggplot2)
ggplot(gpu_col, aes(x = Manufacturer, y=Memory_Speed)) + geom_boxplot() + stat_summary(fun.y = "mean", geom = "point", color = "red") + theme_minimal()
##toc do xu li anh pixel va texel
pF_sum <- gpu_col %>%
	group_by(Manufacturer) %>%
	summarize(
		 sample_size = n(),
		 mean = mean(Pixel_Rate, na.rm = TRUE),
		 sd = sd(Pixel_Rate, na.rm = TRUE),
		 minimum = min(Pixel_Rate, na.rm = TRUE),
		 first_quantile = quantile(Pixel_Rate, 0.25, na.rm=TRUE),
		 median = median(Pixel_Rate, na.rm = TRUE),
		 third_quantile = quantile(Pixel_Rate, 0.75, na.rm=TRUE),
		 maximum = max(Pixel_Rate, na.rm = TRUE)
		 )
head(pF_sum)
tF_sum <- gpu_col %>%
  group_by(Manufacturer) %>%
  summarize(
    sample_size = n(),
    mean = mean(Texture_Rate, na.rm = TRUE),
    sd = sd(Texture_Rate, na.rm = TRUE),
    minimum = min(Texture_Rate, na.rm = TRUE),
    first_quantile = quantile(Texture_Rate, 0.25, na.rm = TRUE),
    median = median(Texture_Rate, na.rm = TRUE),
    third_quantile = quantile(Texture_Rate, 0.75, na.rm = TRUE),
    maximum = max(Texture_Rate, na.rm = TRUE)
  )
head(tF_sum)
#ve do thi boxplot cho ca hai pixel, texel
ggplot(gpu_col, aes(x = Manufacturer, y = Pixel_Rate)) + geom_boxplot()+ stat_summary(fun.y = "mean", geom = "point", color ="red") + theme_minimal()
ggplot(gpu_col, aes(x = Manufacturer, y = Texture_Rate)) + geom_boxplot() + stat_summary(fun.y = "mean", geom = "point", color ="red")+	theme_minimal()


#memory
#ve do thi scatterplot
library(plotly)
library(anytime)
#tai du lieu
dataset <- gpu_col

# Define a function to count pixels based on Best_Resolution column
countPixels <- function(x) {
  if (is.na(x)) {
    return(800 * 600)
  } else {
    value <- as.numeric(unlist(strsplit(x, 'x')))
    return(value[1] * value[2])
  }
}

# Apply countPixels function to create a new column PixelNum
dataset$PixelNum <- sapply(dataset$Best_Resolution , countPixels)

# Convert Release_Date column to Date format
dataset$Release_Date <- anydate(dataset$Release_Date)
dataset$Release_Date <- as.Date(dataset$Release_Date, format = "%Y-%m-%d")

# Extract Year from Release_Date column
dataset$Year <- format(dataset$Release_Date, "%Y")

# Initialize a plotly object
fig <- plot_ly()

# Loop through unique manufacturers to create scatter plots
for(manufacturer in unique(dataset$Manufacturer)) {
  trace_dataset <- subset(dataset, Manufacturer == manufacturer)
  fig <- fig %>% add_trace(
    x = trace_dataset$Year,
    y = trace_dataset$Memory,
    type = 'scatter',
    mode = 'markers',
    name = manufacturer,
    marker = list(
      symbol = 'circle',
      size = trace_dataset$PixelNum / 100000,
      opacity = 0.1,
      line = list(
        width = 1,
        color = 'rgb(255, 255, 255)'
      )
    ),
    text = trace_dataset$Name
  )
}

# Set layout for the plot
fig <- fig %>% layout(
  title = 'GPU memory vs Year of Release by Manufacturer',
  paper_bgcolor = 'rgb(242, 242, 242)',
  plot_bgcolor = 'rgb(242, 242, 242)',
  yaxis = list(
    title = 'GPU Memory',
    ticklen = 5,
    gridcolor = 'rgb(255, 255, 255)',
    gridwidth = 2
  ),
  xaxis = list(
    title = 'Year of Release',
    ticklen = 5,
    gridcolor = 'rgb(255, 255, 255)',
    gridwidth = 2
  )
)

# Print the plot
fig

#tmus plot
tmu_fig <- plot_ly()

for(manufacturer in unique(dataset$Manufacturer)) {
  trace_dataset <- subset(dataset, Manufacturer == manufacturer)
  tmu_fig <- tmu_fig %>% add_trace(
    x = trace_dataset$Year,
    y = trace_dataset$TMUs,
    type = 'scatter',
    mode ='markers',
    name = manufacturer,
    marker = list(
      symbol = 'circle',
      size = trace_dataset$PixelNum / 100000,
      opacity = 0.1,
      line = list (
        width = 1,
        color = 'rgb(255, 255, 255)'
      )
    ),
    text = trace_dataset$Name
  )
}

tmu_fig <- tmu_fig %>% layout(
  title = 'TMUs year of release by Manufacturer',
  paper_bgcolor = 'rgb(240, 240, 240)',
  plot_bgcolor = 'rgb(240, 240, 240)',
  yaxis = list(
    title = 'TMUs of the GPU',
    ticklen = 5,
    gridcolor = 'rgb(255, 255, 255)',
    gridwidth = 2
  ),
  xaxis = list(
    title = 'Year of release',
    ticklen = 5,
    gridcolor = 'rgb(255, 255, 255)',
    gridwidth = 2
  )
)

tmu_fig

#thiết lập folder làm việc (nơi lưu file data)
setwd("C:/Users/BonBon/OneDrive/Máy tính/xác suất thống kê/")
#đọc dữ liệu 2 file vào
gpus <- read.csv("All_GPUs.csv", header = TRUE)
cpus <- read.csv("Intel_CPUs.csv", header = TRUE)

str(gpus) #in cấu trúc của data (kiểu dữ liệu và thành phần)
str(cpus)
#tạo data mới chỉ gồm các cột cần dùng
df_gpus <- gpus[,c("DVI_Connection","HDMI_Connection","VGA_Connection","TMUs","Open_GL","Shader","Memory","Memory_Speed")]
df_cpus <- cpus[,c("Vertical_Segment","Lithography","nb_of_Cores","nb_of_Threads","Max_Turbo_Frequency")]

head(gpus) #in dữ liệu 6 dòng đầu

apply(is.na(df_gpus), 2, sum) #kiểm tra dữ liệu NA của từng cột trong data
apply(is.na(df_cpus), 2, sum)

print("Xử lý số liệu:")
#GPUs
df_gpus$Memory <- gsub(" MB", '', df_gpus$Memory) #dùng để thay thế ký tự này thành ký tự khác
df_gpus$Memory <- as.numeric(df_gpus$Memory) #ép kiểu dữ liệu thành kiểu số (num)
df_gpus$Memory_Speed <- gsub(" MHz", '', df_gpus$Memory_Speed)
df_gpus$Memory_Speed <- as.numeric(df_gpus$Memory_Speed)
#thay các dữ liệu NA bằng giá trị trung bình
df_gpus$DVI_Connection[is.na(df_gpus$DVI_Connection)] = mean(df_gpus$DVI_Connection, na.rm = TRUE)
df_gpus$HDMI_Connection[is.na(df_gpus$HDMI_Connection)] = mean(df_gpus$HDMI_Connection, na.rm = TRUE)
df_gpus$VGA_Connection[is.na(df_gpus$VGA_Connection)] = mean(df_gpus$VGA_Connection, na.rm = TRUE)
df_gpus$TMUs[is.na(df_gpus$TMUs)] = mean(df_gpus$TMUs, na.rm = TRUE)
df_gpus$Open_GL[is.na(df_gpus$Open_GL)] = mean(df_gpus$Open_GL, na.rm = TRUE)
df_gpus$Shader[is.na(df_gpus$Shader)] = mean(df_gpus$Shader, na.rm = TRUE)
df_gpus$Memory[is.na(df_gpus$Memory)] = mean(df_gpus$Memory, na.rm = TRUE)
df_gpus$Memory_Speed <- gsub(" MHz", '', df_gpus$Memory_Speed)
df_gpus$Memory_Speed <- as.numeric(df_gpus$Memory_Speed)
df_gpus$Memory_Speed[is.na(df_gpus$Memory_Speed)] = mean(df_gpus$Memory_Speed, na.rm = TRUE)
#CPUs
df_cpus$Lithography <- gsub(" nm", '', df_cpus$Lithography)
df_cpus$Lithography <- as.numeric(df_cpus$Lithography)
df_cpus$Lithography[is.na(df_cpus$Lithography)] = mean(df_cpus$Lithography, na.rm = TRUE)

df_cpus$Max_Turbo_Frequency <- gsub(" GHz", '', df_cpus$Max_Turbo_Frequency)
df_cpus$Max_Turbo_Frequency <- as.numeric(df_cpus$Max_Turbo_Frequency)
df_cpus$Max_Turbo_Frequency[is.na(df_cpus$Max_Turbo_Frequency)] = mean(df_cpus$Max_Turbo_Frequency, na.rm = TRUE)
#---------------------------

print("Tìm khoảng tin cậy:")
t.test(df_gpus$Open_GL, conf.level = 0.95)

#---------------------------

print("Kiểm định một mẫu:")
#H0: mu = 3072
#H1: mu < 3072
t.test(df_gpus$Memory, alternative = "less", mu = 3072, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

#---------------------------

print("Kiểm định hai mẫu:")
t.test(df_gpus$Open_GL, df_gpus$Shader, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

#---------------------------

print("Anova mot yếu tố:")
VS <- factor(df_cpus$Vertical_Segment) #biến độc lập
MTF <- df_cpus$Max_Turbo_Frequency #biến phụ thuộc

table(df_cpus$Vertical_Segment) #số lượng mẫu từng biến độc lập

#Kiểm định giả thiết:
# - Các mẫu độc lập
# - Biến phụ thuộc là biến liên tục
# - Các nhóm được lấy ra từ tổng thể có phân phối chuẩn hoặc gần chuẩn

#Kiểm định Shapiro-Wilk để biết có phân phối chuẩn hoặc gần chuẩn:
library(nortest)
shapiro.test(MTF[VS=="Desktop"])
shapiro.test(MTF[VS=="Embedded"])
shapiro.test(MTF[VS=="Mobile"])
shapiro.test(MTF[VS=="Sever"])

plot(aov(MTF~VS), 2) #vẽ đồ thị

av_res <- rstandard(aov(MTF~VS))
shapiro.test(av_res)

library(car)
leveneTest(MTF~VS) #kiểm tra các nhóm có phương sai đồng nhất

#phân tích anova
av <- aov(MTF~VS, data = df_cpus)
summary(av) #hiển thị kết quả

TukeyHSD(av) #phân tích xem có sự khác nhau giữa các biến độc lập

#---------------------------

print("Hồi quy tuyến tính đơn:")
#vẽ đồ thị hồi quy
library(ggpubr)
ggscatter(data = df_gpus, y = "Memory", x = "Memory_Speed") + geom_smooth(method = "lm", se = T)
#hồi quy tuyến tính đơn
lm <- lm(df_gpus$Memory~df_gpus$Memory_Speed, data = df_gpus)
summary(lm)

plot(lm)