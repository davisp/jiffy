#!/bin/sh
#
# Install EQC mini in $project_dir/eqc, where rebar config will find it.
#
# Note: mind what version it's compiled for and make sure to test it on
# Erlang version no more than two major version numbers away from it.
# So, for example, 25 should be good for 25, 26 and 27.
#

set -eu

EQC_VERSION="2.02.0"
EQC_URL="https://www.quviq.com/downloads/eqcR25-${EQC_VERSION}.zip"
EQC_ZIP_DIR="Quviq QuickCheck Mini version ${EQC_VERSION} for OTP 25"

PROJECT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
INSTALL_DIR="${PROJECT_DIR}/eqc"

if [ -d "${INSTALL_DIR}/ebin" ]; then
    echo "Already installed at ${INSTALL_DIR}"
    exit 0
fi

TMPDIR="${TMPDIR:-/tmp}"
ZIP_FILE="${TMPDIR}/eqc-mini-${EQC_VERSION}.zip"
EXTRACT_DIR="${TMPDIR}/eqc-mini-extract-$$"

echo "Downloading ${EQC_URL}"
curl -sfL "${EQC_URL}" -o "${ZIP_FILE}"

rm -rf "${EXTRACT_DIR}"
mkdir -p "${EXTRACT_DIR}"
unzip -q -o "${ZIP_FILE}" -d "${EXTRACT_DIR}"

rm -rf "${INSTALL_DIR}"
mv "${EXTRACT_DIR}/${EQC_ZIP_DIR}/eqc-${EQC_VERSION}" "${INSTALL_DIR}"

rm -rf "${EXTRACT_DIR}" "${ZIP_FILE}"

echo "Installed in ${INSTALL_DIR}"
