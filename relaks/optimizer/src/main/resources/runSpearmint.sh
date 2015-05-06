#! /bin/sh

source /Users/Pietras/.env/anaconda/envs/mgr/bin/activate mgr 2>/dev/null && \
cd /Users/Pietras/mgr/optimizers/spearmint/spearmint-lite-ersatz && \
python spearmint-lite.py $@