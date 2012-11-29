require 'formula'

class Macra < Formula
  homepage ''
  url "https://nodeload.github.com/pasberth/Macra/tar.gz/Formula4Homebrew"
  version '0.0.1'
  sha1 '65f88be9d17b43848d6e9ee79fc85b42e738941b'

  depends_on :autoconf
  depends_on 'ghc'

  def install
    system "autoconf"

    system "./configure", "--prefix=#{prefix}"
    system "make", "compile"
    system "make", "install"
    ENV.prepend 'PATH', prefix/'bin', ':'
  end

  def test
    # This test will fail and we won't accept that! It's enough to just replace
    # "false" with the main program this formula installs, but it'd be nice if you
    # were more thorough. Run the test with `brew test macra`.
    system "false"
  end
end
