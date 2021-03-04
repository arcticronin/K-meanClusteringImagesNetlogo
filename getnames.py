import os

os.path.abspath('')
file_list = os.listdir(os.path.abspath(''))
print (file_list)
file_list.sort()
file_object = open(r"names.txt", "w+" )
for elem in file_list:
    if elem.endswith('jpg')or elem.endswith('png'): 
        print (elem)
        file_object.write(elem + '\n')
file_object.close()