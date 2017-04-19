#-*- coding: UTF-8 -*-
import re
from prettytable import PrettyTable


def read_output(file_path):
    previous_instance_id = None
    instance_list = []
    type_list = []
    element_list = []
    role_list = []

    with open(file_path, "r") as fin:
        for line in fin:
            tuple_elements = line.split(" ")
            instance_id = re.match("<http://framebase.org/ns/fi-(.+)>", tuple_elements[0]).group(1)
            if instance_id != previous_instance_id:
                instance_type = re.match("<http://framebase.org/ns/frame-(.+)>", tuple_elements[2]).group(1)
                previous_instance_id = instance_id
                instance_list.append(instance_id)
                if re.match("Unmapped-.+", instance_type) is None:
                    type_list.append(instance_type)
            else:
                instance_role = re.match("<http://framebase.org/ns/fe-(.+)>", tuple_elements[1]).group(1)
                try:
                    instance_element = re.match("<http://.+/(.+)>", tuple_elements[2]).group(1)
                except:
                    instance_element = ""
                
                role_list.append(instance_role)
                element_list.append(instance_element)

    calculate_statistics(instance_list, type_list, element_list, role_list)

def calculate_statistics(instance_list, type_list, element_list, role_list):
    table = PrettyTable()
    table.field_names = ["Item", "All values", "Unique values"]
    table.add_row(["Instances", len(instance_list), len(set(instance_list))])
    table.add_row(["Types", len(type_list), len(set(type_list))])
    table.add_row(["Elements", len(element_list), len(set(element_list))])
    table.add_row(["Roles", len(role_list), len(set(role_list))])
    table.align = "r"
    print table

if __name__ == '__main__':
    knews_path = "ukb_knews.txt"
    framester_path = "ukb_framester.txt"
    read_output(knews_path)
    read_output(framester_path)