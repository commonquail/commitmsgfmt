# This script takes care of building your crate and packaging it for release

set -ex

main() {
    src=$(pwd)
    stage=

    case "$TRAVIS_OS_NAME" in
        linux)
            stage=$(mktemp -d)
            ;;
        osx)
            stage=$(mktemp -d -t tmp)
            ;;
    esac

    package="$CRATE_NAME-$TRAVIS_TAG-$TARGET"
    mkdir "$stage/$package"

    test -f Cargo.lock || cargo generate-lockfile

    cross build --target "$TARGET" --release

    cp LICENSE.txt "$stage/$package"
    cp README.md "$stage/$package"
    cp "target/$TARGET/release/$CRATE_NAME" "$stage/$package"
    cp "doc/$CRATE_NAME.1" "$stage/$package"
    cp -r contrib "$stage/$package"

    cd "$stage"
    strip "$package/$CRATE_NAME"
    tar czf "$src/$package.tar.gz" "$package"
    cd "$src"

    rm -rf "$stage"
}

main
