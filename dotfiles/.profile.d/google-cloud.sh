if which lsb_release >/dev/null; then
    export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -cs)"
fi
