# Akbank Bootcamp Final Projesi

## Giriş ve çıkış dosyaları:
### QSAM.FINALINP:
Programın, hangi işlemi hangi banka hesabına yapacağını aldığı giriş dosyası.
> İçeriği soldan sağa olmak üzere sırayla: İşlem numarası, id, döviz numarası.

### VSAM.FINALVS1:
Bütün banka hesaplarının bilgilerinin indekslenmiş bir şekilde tutulduğu VSAM dosyasıdır. VSAM dosyası olduğu için içerisinde anahtar araması yapabiliyoruz
bu sayede VSAM içerisinde sadece id ve döviz numarasını anahtar olarak alıp arama yapabiliyoruz ve eşleşen değerler bulunduğunda ilgili id ve döviz numarasına ait bütün hesap
bilgilerine ulaşabiliyoruz.
> İçeriği soldan sağa olmak üzere sırayla: İd, döviz numarası, isim, soyisim, son işlem tarihi, hesaptaki toplam para miktarı.

### QSAM.FINALPRT:
Program çalıştıktan sonra oluşan tek çıktı dosyasıdır. Yapılan işlemleri gösterir ve işlemin nasıl gittiğine dair açıklama yazısı bulundurur.
> İçeriği soldan sağa olmak üzere sırayla: İd, döviz numarası, işlemin döndürdüğü kod, eski isim, yeni isim, eski soyisim, yeni soyisim, işlemin açıklaması.

## Programın Genel Akışı
