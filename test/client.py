import json
import socket
import ssl
import struct
import logging
import time
import sys
from kutils.ksonsocket import KSONClientConnection
from threading import Thread

import socket
import fcntl
import struct
import os

token = None
identity = None
role = None

#ENCODER_PIPELINE = "gst-launch-1.0 v4l2src device=/dev/video0 ! 'video/x-raw,width=320,height=240,framerate=30/1' ! x264enc ! h264parse ! rtph264pay ! udpsink host=%s port=%d"
ENCODER_PIPELINE = "gst-launch-1.0 videotestsrc ! x264enc ! h264parse ! rtph264pay ! udpsink host=%s port=%d &"
DECODER_PIPELINE = "gst-launch-1.0 udpsrc port=%d ! application/x-rtp, encoding-name=H264 ! rtph264depay ! decodebin ! videoconvert ! autovideosink sync=false &"

def get_ip_address(ifname):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    return socket.inet_ntoa(fcntl.ioctl(
        s.fileno(),
        0x8915,  # SIOCGIFADDR
        struct.pack('256s', ifname[:15])
    )[20:24])

def establish_call(local_port, remote_ip, remote_port):
    encoder_pipeline = ENCODER_PIPELINE%(remote_ip, int(remote_port))
    decoder_pipeline = DECODER_PIPELINE%int(local_port)
    print encoder_pipeline
    os.system(encoder_pipeline)
    print decoder_pipeline
    os.system(decoder_pipeline)

def _client_default_handler(client_connection, msg):
    print msg
    global token, identity, role
    if msg["message"] == "registration":
        token = msg["token"]
    if msg["message"] == "invite":
        caller = msg["payload"]["from"]
        call_session = msg["payload"]["call_session"]
        video_remote_ip = msg["payload"]["media_attribute"]["video"]["rtp"]["lan"]["ip"]
        video_remote_port = msg["payload"]["media_attribute"]["video"]["rtp"]["lan"]["port"]
        ref = msg["uuid"]
        establish_call(9068, video_remote_ip, video_remote_port)
        client_connection.send_msg(
            {
                "message": "ready",
                "uuid": "79235421-dcf3-470e-97f8-878f04390890",
                "ref": ref,
                "token": token,
                "payload":{
                    "call_session": call_session,
                    "media_attribute":{
                        "audio":{
                            "codec_list":{"1": "speex","2": "pcmu"},
                            "rtp": {
                                "wan":{"ip": "117.82.22.11", "port": 7078},
                                "lan":{"ip": get_ip_address('wlan0'), "port": 7078}
                            }
                        },
                        "video":{
                            "codec_list":{"1": "h264","2": "vp8"},
                            "rtp": {
                                "wan":{"ip": "117.82.22.11", "port": 9068},
                                "lan":{"ip": get_ip_address('wlan0'), "port": 9068}
                            }
                        },
                        "rtp_type":"SRTP",
                    }
                },
            }
        )
    if msg["message"] == "ready":
        video_remote_ip = msg["payload"]["media_attribute"]["video"]["rtp"]["lan"]["ip"]
        video_remote_port = msg["payload"]["media_attribute"]["video"]["rtp"]["lan"]["port"]
        establish_call(9078, video_remote_ip, video_remote_port)
def main(argv):
    global identity, role
    identity = argv[1]
    role = argv[0]
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
    if role == "caller":
        time.sleep(1)

        client_connection.send_msg(
            {
                "message": "invite",
                "uuid": "79235421-dcf3-470e-97f8-878f04390891",
                "token": token,
                "payload":{
                    "to":argv[2],
                    "from":argv[1],
                    "media_attribute":{
                        "audio":{
                            "codec_list":{"1": "speex","2": "pcmu"},
                            "rtp": {
                                "wan":{"ip": "117.82.22.11", "port": 7078},
                                "lan":{"ip": get_ip_address('wlan0'), "port": 7078}
                            }
                        },
                        "video":{
                            "codec_list":{"1": "h264","2": "vp8"},
                            "rtp": {
                                "wan":{"ip": "117.82.22.11", "port": 9078},
                                "lan":{"ip": get_ip_address('wlan0'), "port": 9078}
                            }
                        },
                        "rtp_type":"SRTP",
                    }
                },
            }
        )
    while True:
        time.sleep(1)

if __name__ == "__main__":
    main(sys.argv[1:])
