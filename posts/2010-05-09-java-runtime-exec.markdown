----
title: java//Runtime exec
date: 2010/05/09
author: Alexander Vershilov
tag: java, exec, runtime, process, IO
license: by-nc-sa
----

Поскольку в одной совершенно не требующей этого задаче мне пришлось столкнуться с тем, 
что нужно было осуществить взаимодействие со внешней программой. Я решил в этом немного 
разобраться и натолкнулся на большое количество тем с вопросами, так, 
что тут небольшая подборка всего этого безобразия.

# Запуск внешней программы

запуск внешней программы в Java осуществляется вызовом метода 
`Runtime exec(String[] command,String[] env);`. Передача параметрF массивом нужна, 
что того чтобы с ними было меньше проблем, например с пробелами.
Ожидание завершения программы производится вызовом метода `int waitFor(0)` и мы 
получаем возвращаемое процессом значение. Так же есть методы:

  * `destoy` - завершение процесса, и
  * `exitValue` – возвращающий значение выхода или исключение, если процесс не завершён.

Итого код примера:

    String cmd = {"cat","--help"};
    Process pCat = System.getRuntime().exec(cmd);
    pCat.waitFor();

# Перенаправление потока внешней программы

Часто перед нами стоит задача получение доступа к потокам ввода-вывода программы. 
Это делается следующими методами:

<table>
  <tr><td>Метод</td><td>Возвращаемый тип</td><td>что такое</td></tr>
  <tr><td>getOutputStream</td><td>   OutputStream</td><td>  поток ввода программы (stdin)</td></tr>
  <tr><td>getInputStream</td><td>  InputStream</td><td>   поток вывода программы(stdout)</td></tr>
  <tr><td>getErrorStream</td><td>  InputStream</td><td>   поток ошибок программы(stderr)</td></tr>
</table>

NOTE Да, названия методов немного не логичны.

Пример получения потоков:


    stdin = pCat.getOutputStream();
    stdout = pCat.getInputStream();
    stderr = pCat.getErrorStream();
    
    String line =”hello world” + “\n”;
    stdin.write(line.getBytes()); //отправляем данные программе
    stdin.flush();
    stdin.close();
    
    BufferedReader outReader = new BufferedReader(new InputStreamReader(stdout));
    while ((line = outReader.readLine ()) != null) { //считываем поток выхода
        System.out.println (line);
    }
    outReader.close();
    
    BufferedReader errReader = new BufferedReader(new InputStreamReader(stderr));
    while ((line = errReader.readLine ()) != null) { //считываем поток ошибок
        System.out.println (“[Stderr] ” + line);
    }
    errReader.close();

Замечу, что в данной программе есть налог клавиши ctrl+D это `stdin.close();` =)
