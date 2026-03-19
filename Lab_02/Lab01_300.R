# PHAN 1: LOAD DU LIEU VA KIEM TRA TONG QUAN

# Doc du lieu tu file csv
# encoding = 'latin1' de tranh loi font chu vi ten laptop co nhieu ky tu la
laptops <- read.csv("D:\\Cong_viec\\R\\lab02\\laptops.csv", stringsAsFactors = FALSE, encoding = "latin1")

# Xem nhanh bang du lieu (mo tab moi nhu Excel)
View(laptops)

# Xem 6 dong dau tien de biet du lieu trong nhu the nao
head(laptops)

# Xem 10 dong cuoi cung
tail(laptops, 10)

# Kiem tra cau truc du lieu
str(laptops)

table(laptops$Ram)

# Dung gsub de thay the chuoi "GB" bang rong ""
laptops$Ram <- gsub("GB", "", laptops$Ram)

# Chuyen doi kieu du lieu sang Numeric
laptops$Ram <- as.numeric(laptops$Ram)

# Kiem tra lai xem da la kieu so chua
is.numeric(laptops$Ram)
summary(laptops$Ram)

# Xu ly cot Weight
# Tuong tu nhu RAM, cot Weight chua chu "kg"
# Kiem tra cac gia tri la (co the co may ghi bang don vi khac)
head(laptops$Weight, 10)

# Loai bo chu "kg"
laptops$Weight <- gsub("kg", "", laptops$Weight)

# Ep kieu ve so
laptops$Weight <- as.numeric(laptops$Weight)

# Kiem tra cac may nang bat thuong (> 4kg)
laptops[laptops$Weight > 4 & !is.na(laptops$Weight), ]

# Xem ten cua tat ca cac cot
names(laptops)

# Kiem tra kich thuoc cua dataset
# (So dong) (So cot)
dim(laptops)

# Loai bo cot dau tien (Cot ID khong co gia tri phan tich)
# Kiem tra thu cot 1 truoc khi xoa
head(laptops[, 1]) 

# Tien hanh xoa cot 1
laptops <- laptops[, -1]

# Kiem tra lai ten cac cot sau khi xoa
colnames(laptops)

# Cho ta biet gia min, max, trung binh cua cac cot so
summary(laptops)

# Kiem tra chi tiet bien Inches (Kich thuoc man hinh)
# Xem danh sach cac kich thuoc man hinh tu nho den lon
sort(unique(laptops$Inches))

# Dem so luong may cho tung kich thuoc man hinh
table(laptops$Inches)

# Kiem tra phan bo thong ke cua Inches
summary(laptops$Inches)

# Tim cac may co man hinh "khong lo" (tren 17 inch)
laptops[laptops$Inches > 17, ]

# Kiem tra xem co may nao man hinh nho bat thuong khong (duoi 10 inch)
laptops[laptops$Inches < 10, ]

# Kham pha bien "Company" (Hang san xuat)
# Xem co bao nhieu hang laptop trong du lieu
unique(laptops$Company)

# Dem so luong may cua moi hang
table(laptops$Company)

# Xem hang nao xuat hien nhieu nhat (Dung sort de de nhin)
sort(table(laptops$Company), decreasing = TRUE)

# Kham pha bien "Inches" (Kich thuoc man hinh)
# Kiem tra phan bo tuoi
summary(laptops$Inches)

# Tim cac may co man hinh nho nhat (Netbook thuong < 12 inch)
laptops[laptops$Inches < 12, ]

# Kham pha bien "Price_euros" (Gia tien)
# Kiem tra gia trung binh cua laptop
mean(laptops$Price_euros)

# Tim chiec laptop dat nhat trong bo du lieu
laptops[laptops$Price_euros == max(laptops$Price_euros), ]

# Tim chiec laptop re nhat
laptops[laptops$Price_euros == min(laptops$Price_euros), ]

# Phat hien missing data (NA)
# Tim cac dong co du lieu bi trong
laptops[!complete.cases(laptops), ]

# Dem tong so dong bi thieu du lieu
length(laptops[!complete.cases(laptops), ])

# Kiem tra cac o trong nhung khong phai NA (Vi du: "")
sum(laptops$Company == "")

# Kiem tra du lieu bi thieu (NA) tung cot
sum(is.na(laptops$Company))
sum(is.na(laptops$Product))
sum(is.na(laptops$TypeName))
sum(is.na(laptops$Inches))
sum(is.na(laptops$Ram))
sum(is.na(laptops$Weight))
sum(is.na(laptops$Price_euros))

# Xu ly cot Ram (Dang la chu "8GB" -> can chuyen ve so 8)
# Xem thu du lieu cu
head(laptops$Ram)

# Xem ket qua dem nhom
table(laptops$TypeName)

# Dung gsub de xoa chu "GB"
laptops$Ram <- gsub("GB", "", laptops$Ram)

# Ep kieu ve so (numeric)
laptops$Ram <- as.numeric(laptops$Ram)

# Kiem tra lai sau khi chuyen doi
summary(laptops$Ram)

# Xu ly cot Weight (Dang la "1.37kg" -> can ve so 1.37)
laptops$Weight <- gsub("kg", "", laptops$Weight)
laptops$Weight <- as.numeric(laptops$Weight)

# Kiem tra xem co dong nao bi loi NA sau khi chuyen doi khong
sum(is.na(laptops$Weight))

# Bien Company (Thuong hieu)
# Chuyen thanh factor de kiem soat nhan
laptops$Company <- factor(laptops$Company)
summary(laptops$Company)

# Kiem tra chi tiet cho mot hang cu the, vi du: Razer
laptops[laptops$Company == "Razer", ]

# Kiem tra phan vi de thay su phan bo
quantile(laptops$Inches)

# Phan tich bien Price_euros (Gia tien)
# Tinh trung binh cong cua gia may tinh
mean(laptops$Price_euros)

# Tinh trung vi (Median) - gia tri nam chinh giua tap du lieu
median(laptops$Price_euros)

# Tinh do lech chuan (Standard Deviation)
# Do luong muc do chenh lech gia giua cac may
sd(laptops$Price_euros)

# Tim cac may co gia cuc re (duoi 200 Euro)
subset(laptops, Price_euros < 200)

# Tim cac may co gia cuc dat (tren 4000 Euro)
subset(laptops, Price_euros > 4000)

# Bien TypeName (Loai laptop)
# Xem cac gia tri goc
unique(laptops$TypeName)

# Chuyen doi va dat lai ten nhan cho chuyen nghiep
laptops$TypeName <- factor(laptops$TypeName,
                           levels = c("Ultrabook", "Notebook", "Netbook", "Gaming", "2 in 1 Convertible", "Workstation"),
                           labels = c("Sieu mong", "Pho thong", "Netbook", "Gaming", "Cam ung", "Workstation")
)

# Chuyen doi OpSys thanh Factor
# Ta gan nhan ro rang hon cho tung phien ban
laptops$OpSys <- factor(laptops$OpSys,
                        levels = c("macOS", "No OS", "Windows 10", "Mac OS X", 
                                   "Linux", "Android", "Windows 10 S", "Chrome OS", 
                                   "Windows 7"),
                        labels = c("He dieu hanh macOS", 
                                   "Khong co he dieu hanh", 
                                   "Windows 10 Ban quyen", 
                                   "Mac OS X Phien ban cu", 
                                   "He dieu hanh Linux", 
                                   "Nen tang Android", 
                                   "Windows 10 S-Mode", 
                                   "Google Chrome OS", 
                                   "Windows 7 Professional")
)

# Bien OpSys (He dieu hanh)
# Kiem tra cac gia tri hien co
table(laptops$OpSys)

# Kiem tra cau truc Factor co thu tu
str(laptops$Ram)

# Xem tom tat so luong may theo dung luong RAM
summary(laptops$Ram)

# Kiem tra cac may co dung luong RAM thap nhat (2GB)
subset(laptops, Ram == 2)

# Kiem tra cac may co dung luong RAM cao nhat (64GB)
subset(laptops, Ram == 64)

# Sap xep tang dan theo ten Product (A -> Z)
laptops_sorted <- laptops[order(laptops$Product), ]

# Sap xep giam dan theo ten Product (Z -> A)
laptops_sorted_desc <- laptops[order(laptops$Product, decreasing = TRUE), ]

# Sap xep theo Hang truoc, roi moi den ten Product (De cac may cung hang nam gan nhau)
laptops_final <- laptops[order(laptops$Company, laptops$Product), ]

# Xem thu ket qua 10 dong dau
head(laptops_final[, c("Company", "Product")])

# Sap xep tang dan (Tu man hinh nho den man hinh lon)
laptops_inches_asc <- laptops[order(laptops$Inches), ]

# Xem thu 10 dong dau tien (Cac may 10.1, 11.6 inch se hien len truoc)
head(laptops_inches_asc[, c("Company", "Product", "Inches")])

# Sap xep giam dan (Tu man hinh lon den man hinh nho)
# Them dau tru "-" truoc ten cot de dao nguoc thu tu
laptops_inches_desc <- laptops[order(-laptops$Inches), ]

# Xem thu 10 dong dau tien (Cac may 17.3, 18.4 inch se hien len truoc)
head(laptops_inches_desc[, c("Company", "Product", "Inches")])

# Sap xep ket hop (Nhieu cot)
laptops_inches_price <- laptops[order(laptops$Inches), ]

# Kiem tra ket qua
head(laptops_inches_price[, c("Company", "Product", "Inches", "Price_euros")], 20)

# Kiem tra nhanh cac kich thuoc pho bien nhat sau khi sort
summary(laptops$Inches)

# Sap xep theo ten San pham (Product) tu A den Z
laptops_name <- laptops[order(laptops$Product), ]

# Sap xep theo Kich thuoc man hinh (Inches) tu nho den lon
laptops_inches <- laptops[order(laptops$Inches), ]

# Sap xep theo Hang san xuat (Company) tu A den Z
laptops_company <- laptops[order(laptops$Company), ]

# Sap xep ket hop: Theo Hang truoc, trong moi Hang thi may Re nhat xep truoc
laptops_final <- laptops[order(laptops$Company, laptops$Price_euros), ]

# Sap xep toan bo bang theo gia giam dan
laptops_expensive <- laptops[order(-laptops$Price_euros), ]

# Xem 20 dong dau
head(laptops_expensive, 20)

# Kiem tra xem co nhung hang nao
table(laptops$Cpu_Brand)
laptops$Cpu_Brand <- factor(laptops$Cpu_Brand)

# Phan loai o cung SSD hay HDD
# Tao cot moi mac dinh la 0
laptops$is_SSD <- ifelse(grepl("SSD", laptops$Memory), "Yes", "No")
laptops$is_SSD <- factor(laptops$is_SSD)

# Kiem tra man hinh co cam ung (Touchscreen) hay khong
laptops$Touchscreen <- ifelse(grepl("Touchscreen", laptops$ScreenResolution), "Yes", "No")
laptops$Touchscreen <- factor(laptops$Touchscreen)

# Tinh gia trung binh theo tung hang (Dung ham aggregate)
# Rat huu ich cho bao cao
aggregate(Price_euros ~ Company, data = laptops, mean)

# Tinh gia trung binh theo RAM
# De xem RAM cang cao thi gia tang nhu the nao
aggregate(Price_euros ~ Ram, data = laptops, mean)

# Dem so luong laptop theo He dieu hanh va Loai may
table(laptops$OpSys, laptops$TypeName)

# Kiem tra lai cau truc lan cuoi
str(laptops)

# Kiem tra xem con NA khong
sum(is.na(laptops))

# Kiem tra tong ket cuoi cung
# Kiem tra so dong va so cot lan cuoi
nrow(laptops)
ncol(laptops)

# Luu du lieu da lam sach ra file moi
# row.names = FALSE de khong luu them cot so thu tu du thua
write.csv(laptops, "laptops_clean_v1.csv", row.names = FALSE)