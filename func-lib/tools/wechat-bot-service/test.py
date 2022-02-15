import requests
import json

APPID = "ufZY9s5A7osdM24"
TOKEN = "dCtPyTN3S57GvV9AmPMii1gRKAmtkm"

username = ""
userid = "test"
query = "今日天气"

# 请求signature
sign_url = f"https://openai.weixin.qq.com/openapi/sign/{TOKEN}"
data = {"username":username,
        "avatar":"",
        "userid":userid}

sign_res = requests.post(url=sign_url,data=data)
sign = json.loads(sign_res.text)

#print(sign["signature"])

# 请求对话query
talk_url = f"https://openai.weixin.qq.com/openapi/aibot/{TOKEN}"

data = {"signature":sign["signature"],
        "query": query}

ans_res = requests.post(url=talk_url,data=data)
ans = json.loads(ans_res.text)


print(ans["answer"])