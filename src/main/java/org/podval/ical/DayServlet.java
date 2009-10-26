package org.podval.ical;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.ServletException;

import java.io.PrintWriter;
import java.io.IOException;
import java.util.Enumeration;


public class DayServlet extends HttpServlet {

    /**
     * Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doGet(final HttpServletRequest request, final HttpServletResponse response)
        throws ServletException, IOException
    {
            response.setContentType("text/html");
            final PrintWriter out = response.getWriter();
            out.println("<html>");
            out.println("<head>");
            out.println("</head>");
            out.println("<body>");

            final Enumeration<String> attributes = request.getAttributeNames();
            while (attributes.hasMoreElements()) {
                final String attribute = attributes.nextElement();
                out.println("Attribute " + attribute + " = " + request.getAttribute(attribute));
            }

            final Enumeration<String> parameters = request.getParameterNames();
            while (parameters.hasMoreElements()) {
                final String parameter = parameters.nextElement();
                out.println("Parameter " + parameter + " = " + request.getParameter(parameter));
            }

            final Enumeration<String> headers = request.getHeaderNames();
            while (headers.hasMoreElements()) {
                final String header = headers.nextElement();
                out.println("Header " + header + " = " + request.getHeader(header));
            }

            out.println("");
            out.println("</body>");
            out.println("</html>");
    }


//    /**
//     * Handles the HTTP <code>POST</code> method.
//     * @param request servlet request
//     * @param response servlet response
//     * @throws ServletException if a servlet-specific error occurs
//     * @throws IOException if an I/O error occurs
//     */
//    @Override
//    protected void doPost(HttpServletRequest request, HttpServletResponse response)
//    throws ServletException, IOException {
//        processRequest(request, response);
//    }


    /**
     * Returns a short description of the servlet.
     * @return a String containing servlet description
     */
    @Override
    public String getServletInfo() {
        return "books browsing servlet";
    }
}
