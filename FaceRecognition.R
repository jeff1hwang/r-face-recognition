library(httr)
library(jsonlite)
library(base64enc)

# 输入API key 和 Secret Key
api_key <- readline(prompt = "Please paste the API Key: ")
secret_key <- readline(prompt = "Please paste the Secret Key: ")

# 拼接URL
url1 <- paste0("https://aip.baidubce.com/oauth/2.0/token?grant_type=client_credentials&client_id=",
               api_key, "&client_secret=", secret_key)

# 获取Access Token
response <- POST(url = url1)
access_token <- content(response)$access_token

# 将图片转换为base64格式
image_name <- readline("Please enter the image name (xxx.jpg/xxx.png): ")
library(base64enc)
img_base64 <- base64enc::base64encode(what = image_name)

# 上传图片并取回数据
url2 <- url2 <- paste0("https://aip.baidubce.com/rest/2.0/face/v3/detect?access_token=",
                       access_token) # 拼接请求URL和token

raw_data <- POST(url = url2, body = list(image = img_base64,
                                         image_type = "BASE64",
                                         face_field = "gender,age,beauty,gender,race,emotion,face_shape",
                                         face_type = "LIVE"))

raw_data_2 <- content(raw_data) # 使用content读取返回的JSON

# 添加if 判断 error code是否为0，返回error code和error msg
if (raw_data_2$error_code == 0) {
  
  # 是人脸的概率
  face_probability <- raw_data_2$result$face_list[[1]]$face_probability
  cat("图片是人脸的概率：", face_probability*100, "%", "\n")
  
  # 性别
  gender <- raw_data_2$result$face_list[[1]]$gender
  if (gender$type == "female") {
    cat("性别：女 - 准确性：", gender[[2]]*100, "%", "\n")
  } else if (gender$type == "male") {
    cat("性别：男 - 准确性：", gender[[2]]*100, "%", "\n")
  }
  
  # 年龄预测
  age <- raw_data_2$result$face_list[[1]]$age
  cat("预测年龄为：", age, "\n")
  
  # 人种肤色
  race <- raw_data_2$result$face_list[[1]]$race$type
  cat("预测人种肤色：", race, "\n")
  
  # 预测心情·
  emotion <- raw_data_2$result$face_list[[1]]$emotion
  if (emotion$type == "angry") {
    cat("预测心情：愤怒\n")
  }
  if (emotion$type == "disgust") {
    cat("预测心情：厌恶\n")
  }
  if (emotion$type == "fear") {
    cat("预测心情：恐惧\n")
  }
  if (emotion$type == "happy") {
    cat("预测心情：高兴\n")
  }
  if (emotion$type == "sad") {
    cat("预测心情：伤心 \n")
  }
  if (emotion$type == "surprise") {
    cat("预测心情：惊讶 \n")
  }
  if (emotion$type == "neutral") {
    cat("预测心情：中性，无表情\n")
  }
  if (emotion$type == "pouty") {
    cat("预测心情：撅嘴\n")
  }
  if (emotion$type == "grimace") {
    cat("预测心情：鬼脸\n")
  }
  
  # 颜值打分
  beauty <- raw_data_2$result$face_list[[1]]$beauty
  cat("颜值打分(0-100分)：", beauty, "\n")
  
  # 脸型判断
  face_shape <- raw_data_2$result$face_list[[1]]$face_shape
  if (face_shape$type == "square") {
    cat("脸型：方脸\n")
  }
  if (face_shape$type == "triangle") {
    cat("脸型：三角形\n")
  }
  if (face_shape$type == "oval") {
    cat("脸型：椭圆\n")
  }
  if (face_shape$type == "heart") {
    cat("脸型：心形\n")
  }
  if (face_shape$type == "round") {
    cat("脸型：圆形\n")
  }
} else {
  cat("错误\n")
  cat("Error Code:", raw_data_2$error_code, "\n")
  cat("Error message:", raw_data_2$error_msg)
}