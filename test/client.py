import json
import socket
import ssl
import struct
import logging
import time
import sys
from kutils.ksonsocket import KSONClientConnection
from threading import Thread

token = None

def _client_default_handler(json_connection, msg):
    print msg
    global token
    if msg["message"] == "registration":
        token = msg["token"]
    if msg["message"] == "invite":
        token = msg["token"]

def main(argv):
    client_connection = KSONClientConnection(port=8080, secure=False, handler=_client_default_handler)
    client_connection.connect()
    client_connection.start()
    client_connection.send_msg(
        {
            "message": "register",
            "username": argv[1],
            "password": "password"
        }
    )
    if argv[0] == "caller":
        time.sleep(1)
        client_connection.send_msg(
            {
                "message": "invite",
                "uuid": "79235421-dcf3-470e-97f8-878f04390891",
                "token": token,
                "header":{
                          "to":argv[2],
                          "from":argv[1],
                          "media_attribute":{
                              "audio":{"codec_list":["speex","pcmu"], "rtp":7078},
                              "video":{"codec_list":["VP8","H264"], "rtp":9078},
                              "rtp_type":"SRTP",
                              "ip": {"WAN":"117.82.22.11", "LAN":"192.168.2.100"}
                          }
                      },
            }
        )
    while True:
        time.sleep(1)

if __name__ == "__main__":
    main(sys.argv[1:])
