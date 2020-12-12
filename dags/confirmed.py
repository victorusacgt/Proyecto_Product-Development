import os
import pandas as pd
from airflow import DAG
from airflow.contrib.hooks.fs_hook import FSHook
from airflow.contrib.sensors.file_sensor import FileSensor
from airflow.hooks.mysql_hook import MySqlHook
from airflow.operators.python_operator import PythonOperator
from airflow.utils.dates import days_ago


FILE_CONNECTION_ID = "fs_default"
FILE_NAME = "time_series_covid19_confirmed_global.csv"
OUTPUT_TRANSFORM_FILE = "_confirmed_tmp.csv"

dag = DAG('confirmed_dag', description='ETL for confirmed table cases',
          default_args={
              'owner': 'elda.calderon',
              'depends_on_past': False,
              'max_active_runs': 1,
              'start_date': days_ago(5)
          },
          schedule_interval='0 1 * * *',
          catchup=False)

file_sensor_task = FileSensor(dag = dag,
                              task_id = 'file_sensor',
                              fs_conn_id= FILE_CONNECTION_ID,
                              filepath= FILE_NAME,
                              poke_interval= 10,
                              timeout= 300
                              )

def transform_func(**kwargs):
    folder_path = FSHook(conn_id = FILE_CONNECTION_ID).get_path()
    file_path = f"{folder_path}/{FILE_NAME}"
    destination_file = f"{folder_path}/{OUTPUT_TRANSFORM_FILE}"
    df = pd.read_csv(file_path, header = 0, encoding= 'ISO-8859-1')
    df.to_csv(destination_file, index = False)
    os.remove(file_path)
    return destination_file


transform_process = PythonOperator(dag = dag,
                                   task_id = 'transform_process',
                                   python_callable = transform_func,
                                   provide_context= True
                                   )

def insert_process(**kwargs):
    ti = kwargs['ti']
    source_file = ti.xcom_pull(task_ids = 'transform_process')
    db_connection = MySqlHook('airflow_db').get_sqlalchemy_engine()

    df = pd.read_csv(source_file)

    with db_connection.begin() as transaction:
        transaction.execute("DELETE FROM test.confirmed_table WHERE 1=1")
        df.to_sql("confirmed_table", con = transaction, schema = "test", if_exists= "replace", index = False)

    os.remove(source_file)

insert_process = PythonOperator(dag = dag,
                                task_id= 'insert_process',
                                provide_context= True,
                                python_callable = insert_process
                                )

file_sensor_task >> transform_process >> insert_process