require 'formula'

class Macra < Formula
  homepage ''
  url 'https://github.com/pasberth/Macra/archive/macra-0.0.1.tar.gz'
  version '0.0.1'
  sha1 'a13c66960bb9956283aade7f4c00ad2ef8270885'

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
