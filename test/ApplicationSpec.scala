import java.io.File
import models.User
import play.api.db.DBPlugin
import play.api.db.evolutions.{InvalidDatabaseRevision, Evolutions, Evolution, OfflineEvolutions}
import play.api.mvc._
import play.api.test._
import play.api.test.MockApplication._

import org.specs2.mutable._

/**
 * テスト時のHTTPリクエスト生成に使う。
 * Map[String, Seq[String]] をHeadersへラップする。
 */
case class FakeHeaders(map: Map[String, Seq[String]] = Map.empty) extends Headers {

  def getAll(key: String) = map(key)
  def keys = map.keySet
}

/**
 * テスト時のHTTPリクエスト生成に使う。
 * Map[String, Cookie] をCookiesヘラップする
 */
case class FakeCookies(map: Map[String, Cookie] = Map.empty) extends Cookies {

  def get(name: String) = map.get(name)
}

/**
 * テスト用のHTTPリクエスト。
 *
 * 使用例：
 *
 * GETリクエスト
 * FakeRequest("GET", "/login", body = AnyContentAsEmpty)
 *
 * POSTリクエスト
 * FakeRequest("POST", "/login", 
 */
case class FakeRequest[A](
  method: String,
  path: String,
  params: Map[String, Seq[Any]] = Map[String, Seq[Any]](),
  headers: Headers = FakeHeaders(Map.empty),
  cookies: Cookies = FakeCookies(Map.empty),
  body: A,
  scheme: String = "http",
  host: String = "localhost:9000")
  extends Request[A] {

  private def urlEncode(str: String) = java.net.URLEncoder.encode(str, "utf-8")
  def uri = scheme + "://" + host + "/" + path + "?" + params.map {
    t => t._2.flatMap(value => urlEncode(t._1) + "=" + urlEncode(value.toString))
  } mkString("&")
  def queryString = params.mapValues(seqAny => seqAny.map(_.toString))
}

object ApplicationSpec extends Specification {

  /**
   * 実行中アプリケーションのデータソースに対して必要なevolutionスクリプトを取得して、全て適用します。
   * テスト中にDBアクセスをするような場合は、必ず必要なプロセスです。
   *
   * withApplication内で実行する想定で実装しています。
   */
  private def applyAllEvolutionScripts() {
    val app = play.api.Play.current
    val api = app.plugin[DBPlugin].map(_.api).getOrElse(throw new Exception("there should be a database plugin registered at this point but looks like it's not available, so evolution won't work. Please make sure you register a db plugin properly"))
    val db = "default"
    val appPath = new File(".")
    val scripts = Evolutions.evolutionScript(api, appPath, db)
    Evolutions.applyScript(api, db, scripts)
  }

  /**
   * "InvalidDatabaseRevision: Database 'default' needs evolution! [An SQL script need to be run on your database.]"
   * というevolution未適用エラー時にevolutionを実行してからテストを再実行します。
   *
   * ちなみに、たまにevolutionを適用したのに、テーブルが作成されずエラーになることがあります。
   * (そのときのエラー例：　JdbcSQLException: テーブル "USER" が見つかりません）
   * タイミングの問題？
   *
   * テスト実行前にevolutionを適用することがこのメソッドは不要です。
   * しかし、インメモリデータベースを利用している場合、現状のPlay 2.0ではwithApplicationによるアプリ実行時にevolutionの必要性チェックが入るため無理です。
   * というわけで、メソッドでなんとかやっつけます。
   */
  private def withEvolutions[T](spec: => T): T = {
    try {
      spec
    } catch {
      case e: InvalidDatabaseRevision => {
        play.api.Logger.info("Applying all evolution scripts.")
        applyAllEvolutionScripts()
        spec
      }
    }
  }

  "an Application" should {

    /**
     * GET /login へアクセスすると、ログインフォームが表示される。
     */
    "show login page" in {
      withEvolutions {
        withApplication(Nil, MockData.dataSource) {
          // Controllerに対するspecは、
          // 1. requestをテスト対象のactionへ渡して、resultを得る
          // 2. resultからレスポンス内容をextractする
          // 3. レスポンス内容をspecs2のMatcherで検証する
          // という流れになります。
          val action = controllers.Application.login
          val result = action.apply(FakeRequest("GET", "/login", body = AnyContentAsEmpty))
          val extracted = Extract.from(result)
          // ステータスコード
          extracted._1 must equalTo (200)
          // レスポンスヘッダ
          extracted._2.toString mustEqual ("Map(Content-Type -> text/html; charset=utf-8)")
          // レスポンスボディ
          extracted._3 must contain ("/login")
        }
      }
    }

    /**
     * /login へ、emailやpasswordを指定せずにPOSTすると400 Bad Requestになる。
     */
    "does not authenticate" in {
      withEvolutions {
        withApplication(Nil, MockData.dataSource + ("application.secret" -> "appsecret")) {
          val action = controllers.Application.authenticate
          val result = action.apply(FakeRequest("POST", "/login", body = AnyContentAsEmpty))
          val extracted = Extract.from(result)
          extracted._1 must equalTo (400)
        }
      }
    }

    "authenticate" in {
      val config = MockData.dataSource + ("application.secret" -> "appsecret")

      withEvolutions {
        withApplication(Nil, config) {

          User.create(User("mumoshu@sample.com", "mumoshu", "secret"))

          val action = controllers.Application.authenticate
          val result = action.apply(FakeRequest("POST", "/login", body = AnyContentAsUrlFormEncoded(Map(
            "email" -> Seq("mumoshu@sample.com"),
            "password" -> Seq("secret")
          ))))
          val extracted = Extract.from(result)
          extracted._1 must equalTo (302)
        }
      }
    }

    /**
     * GET /logout へアクセスすると、ログインページへリダイレクトされる。
     */
    "logout" in {
      withEvolutions {
        withApplication(Nil, MockData.dataSource) {
          val action = controllers.Application.logout
          val result = action.apply(FakeRequest("GET", "/logout", body = AnyContentAsEmpty))
          val extracted = Extract.from(result)
          extracted._1 must equalTo (302)
        }
      }
    }
  }
}
