# Copyright 2020-2021 egsnrc2py authors. See LICENSE file for details.

import logging
import logging.config
from pathlib import Path

from egsnrc2py._version import __version__, __version_info__

dir_path = Path(__file__).resolve().parent
logging.config.fileConfig(dir_path / "logging.conf")

# create logger
logger = logging.getLogger("egsnrc2py")
