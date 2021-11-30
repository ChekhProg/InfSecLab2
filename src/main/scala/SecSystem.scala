import io.circe.generic.auto._
import io.circe.parser._
import io.circe._

import java.io.FileWriter
import java.io.PrintWriter

import java.io.File
import scala.io.StdIn.readLine

class SecSystem {
  def operate(user: User, lastPath: String): Unit = {
    var path = lastPath
    print(path)
    val line = readLine().trim
    val cmd = if (line.split(" ").isEmpty) "" else line.split(" ")(0)
    cmd match {
      case "" => ()
      case "exit" => return
      case "perms" => println(user.permissions)
      case "write" =>
        val arg = if (line.split(" ").length < 2) None else Some(line.split(" ")(1))
        arg match {
          case None => println("Нет аргумента")
          case Some(v) =>
            val parts = path.split("/").toSeq
            if (parts.isEmpty) {
              val txt = readLine("Введите текст: ")
              val fileWriter: FileWriter = new FileWriter("src/main/resources/system" + path + v)
              fileWriter.write(txt)
              fileWriter.close()
              println("Файл успешно записан")
            }
            else parts(1) match {
              case "data1" if user.permissions.data1 != "WR" => println("У вас нет прав на запись в этой директории")
              case "data2" if user.permissions.data2 != "WR" => println("У вас нет прав на запись в этой директории")
              case "data3" if user.permissions.data3 != "WR" => println("У вас нет прав на запись в этой директории")
              case _ =>
                println(parts.head)
                val txt = readLine("Введите текст: ")
                val fileWriter: FileWriter = new FileWriter("src/main/resources/system" + path + v)
                fileWriter.write(txt)
                fileWriter.close()
                println("Файл успешно записан")
            }
        }
      case "read" =>
        val arg = if (line.split(" ").length < 2) None else Some(line.split(" ")(1))
        arg match {
          case None => println("Нет аргумента")
          case Some(v) =>
            val file = new File("src/main/resources/system" + path)
            val files = file.listFiles.filter(_.isFile).map(_.getName)
            if (files.contains(v)) {
              val source = scala.io.Source.fromFile("src/main/resources/system" + path + v)
              val txt = try source.mkString finally source.close()
              println(s"Файл ${v}: ")
              println(txt)
            }
            else println("Неверный путь. Укажите имя файла")
        }
      case "getlogin" => println(user.login)
      case "login" => login()
      case "list" =>
        val file = new File("src/main/resources/system" + path)
        val these = file.listFiles.map(x => x.getName)
        these.foreach(x => println(x))
      case "cd" =>
        val arg = if (line.split(" ").length < 2) None else Some(line.split(" ")(1))
        arg match {
          case None => println("Нет аргумента")
          case Some(v) if v == ".." =>
            val parts = path.split("/")
            val newPath = parts.dropRight(1).mkString("/", "/", "/")
            val newPath2 = if (newPath.length == 2) "/" else newPath
            path = newPath2
          case Some(v) =>
            val file = new File("src/main/resources/system" + path)
            val directories = file.listFiles.filter(_.isDirectory).map(_.getName)
            if (directories.contains(v)) {
              v match {
                case "data1" if user.permissions.data1 == "N" => println("У вас нет прав на доступ к этой директории")
                case "data2" if user.permissions.data2 == "N" => println("У вас нет прав на доступ к этой директории")
                case "data3" if user.permissions.data3 == "N" => println("У вас нет прав на доступ к этой директории")
                case _ => path = path + v + "/"
              }
            }
            else println("Неверный путь. Укажите следующую директорию без знака '/'")
        }
      case "help" => println("Помощь:\nhelp -> Помощь\nexit -> Выйти\nperms -> Вывести права доступа\n" +
        "write -> Создание и запись файла, требуется аргумент\nread -> Чтение файла, требуется аргумент\n" +
        "getlogin -> Вывести логин\nlogin -> Зайти под другим именем\nlist -> Вывести список файлов и дириктори\n" +
        "cd -> Переход в директорию, требуется аргумент")
      case _ => println(s"Неверная команда: '${cmd}' ")
    }
    operate(user, path)
  }
  def getAccounts: Seq[User] = {
    val source = scala.io.Source.fromFile("src/main/resources/accounts.json")
    val accounts = try source.mkString finally source.close()
    val decoded = decode[Seq[User]](accounts).getOrElse(None)
    decoded.iterator.toSeq
  }
  def login(): Unit = {
    println("Вход в систему")
    println("Введите логин: ")
    val login = readLine("/")
    println("Введите пароль: ")
    val password = readLine("/")
    val accounts = getAccounts
    accounts.find(x => x.login == login) match {
      case None => println("\nПользователь не найден")
      case Some(user) =>
        if (password == user.password) {
          println("\nВход выполнен")
          operate(user, "/")
        }
        else println("\nНеправильный пароль")
    }
  }
}
